;;;;
;;;; A simple concretizing verifier - can handle loop-free programs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.verifiers)

(defclass concretizing-verifier () ())
(defparameter *concretizing-verifier-instance* (make-instance 'concretizing-verifier))

(defun query-smt (body-defns constraint produce-cex)
  (smt:with-solver* (solver smt::*cvc4*)
    (smt:with-scope (solver)
      (smt:dump-commands solver body-defns)

      (smt:declare-constants solver constraint)
      (smt:add-assertion solver (smt:$not constraint))

      (let ((q-res (smt:check-sat solver)))
        (cond
          ((eql :unknown q-res)
           (list :unknown nil))
          ((eql :unsat q-res)
           (list :valid nil))
          ((eql :sat q-res)
           (list :invalid (and produce-cex (smt:get-model solver))))
          (t (error "Invalid SMT response: ~s" q-res)))))))

(defgeneric convert-constraints (spec context)
  (:documentation "Converts SPEC into SMT constraints"))

(defun %do-descriptor-name-update (descriptor new-name expr ctx)
  "Updates a descriptor application"
  (declare (ignore descriptor)) ;; ?? because we only call on the right thing
  (make-instance 'smt::expression
                 :name (smt:ensure-identifier
                        new-name
                        ctx)
                 :arity (1- (smt:arity expr))
                 :sort (smt:sort expr)
                 :children (rest (smt:children expr))
                 :child-sorts (rest (smt:child-sorts expr))))

(defmethod convert-constraints ((spec spec:relational-specification) context)
  "Converts a relational specification into an SMT constraint"
  (let ((rel (spec:expression spec)))
    (loop for descriptor in (spec:descriptors spec)
          do (setf rel (smt:update-expression
                        (a:curry #'%do-descriptor-name-update descriptor "ASDF")
                        rel
                        :context context
                        :filter #'(lambda (expr ctx)
                                    (declare (ignore ctx))
                                    (smt:is-application? expr descriptor)))))
    rel))


(defmethod convert-constraints ((spec spec:intersection-specification) context)
  "Converts an intersection specification into an SMT constraint"
  (apply #'smt:$and (map 'list (a:rcurry #'convert-constraints context)
                         (spec:components spec))))

(defmethod convert-constraints ((spec spec:union-specification) context)
  "Converts a union specification into an SMT constraint"
  (apply #'smt:$or (map 'list (a:rcurry #'convert-constraints context)
                        (spec:components spec))))

(defun %substitute-application (relation new-name)
  "Creates a new application from a relation and new name for the application"
  (let ((term-ix (chc:term-index (chc:head relation)))
        (old-arity (length (chc:actuals relation)))
        (new-actuals nil)
        (new-signature nil))
    (loop for i from 0 below old-arity
          unless (= i term-ix)
            collect (elt (chc:actuals relation) i) into new-a
            and
              collect (elt (chc:signature relation) i) into new-s
          finally (setf new-actuals new-a new-signature new-s))

    (make-instance 'smt::expression
                   :name new-name
                   :sort smt:*bool-sort*
                   :arity (1- old-arity)
                   :children (map 'list #'smt:variable new-actuals new-signature)
                   :child-sorts new-signature)))

(defun %wrap-in-auxiliaries (chc expr)
  "Wraps an SMT expression EXPR in a binding for auxiliary variables in CHC"
  (if (zerop (length (chc:auxiliary-symbols chc)))
      expr
      (smt::quantifier-expression
       "exists"
       (map 'list #'chc:symbol-name (chc:auxiliary-symbols chc))
       (map 'list #'chc:symbol-sort (chc:auxiliary-symbols chc))
       expr)))

(defun concretize-node-chc (node chc semantics context)
  "Concretizes a program node for a specific CHC. Returns a list of created definitions
as the first value, and the concretized CHC body as the second value."
  ;; Handle children first
  (let ((body-applications nil)
        (child-defns nil))
    (loop for child-sym across (chc:child-symbols chc)
          for ix = (chc:symbol-index child-sym)
          for child-node = (elt (ast:children node) ix)
          for sym-name = (chc:symbol-name child-sym)
          for body-rel = (find sym-name
                               (chc:body chc)
                               :key #'(lambda (rel)
                                        (elt (chc:actuals rel)
                                             (chc:term-index (chc:head rel)))))
          ;; body-rel may be null if a child term doesn't appear in the body
          when body-rel
            do (multiple-value-bind (defns new-name)
                   (concretize-program-atom child-node
                                            (chc:name body-rel)
                                            semantics
                                            context)
                 (push (%substitute-application body-rel new-name) body-applications)
                 (setf child-defns (append child-defns defns))))

    ;; Create and return body
    (values
     child-defns
     (%wrap-in-auxiliaries
      chc
      (if (zerop (length body-applications))
          (chc:constraint chc)
          (apply #'smt:$and (chc:constraint chc) body-applications))))))

(defun concretize-program-atom (atom descriptor semantics context)
  "Concretizes a program atom, either a node or a hole."
  (etypecase atom
    (ast:program-node
     (concretize-program-node atom descriptor semantics context))
    (ast:program-hole
     (concretize-program-hole descriptor context))))

(defun concretize-program-node (node descriptor semantics context)
  "Concretizes a program node. Returns a list of created function definition as the
first value, and the concrete function name as the second value."
  ;; Handle children first
  (let* ((child-defns nil)
         (chcs (ast:relational-semantics-for-production semantics
                                                        descriptor
                                                        (ast:production node)))
         (chc-exprs (loop with defns and chc-expr
                          for chc in chcs
                          do (setf (values defns chc-expr)
                                   (concretize-node-chc node chc semantics context))
                          doing (setf child-defns (append child-defns defns))
                          collecting chc-expr)))
    ;; Fixup for multiple CHCs
    (let ((body
            (if (= 1 (length chc-exprs))
                (first chc-exprs)
                (apply #'smt:$or chc-exprs))))

      ;; Write function definition
      (%write-concrete-function-definition (chc:head (first chcs)) body child-defns))))

(defun concretize-program-hole (descriptor context)
  (let ((head (semgus:lookup-head descriptor context)))
    (%write-concrete-function-definition head (smt:$true) nil)))

(defun %write-concrete-function-definition (head body child-defns)
  (let ((term-ix (chc:term-index head))
        (new-sig nil)
        (new-formals nil))
    (loop for i from 0 below (length (chc:formals head))
          unless (= i term-ix)
            collect (elt (chc:formals head) i) into new-f
            and
              collect (elt (chc:signature head) i) into new-s
          finally (setf new-sig new-s new-formals new-f))

    (let ((new-name (smt:ensure-identifier (symbol-name (gensym "FUN-")))))
      (values
       (append child-defns
               (list (smt:to-smt
                      (smt::function-declaration
                       (smt:identifier-smt new-name)
                       new-sig
                       smt:*bool-sort*
                       (map 'list #'smt:variable
                            (map 'list #'smt:identifier-smt
                                 new-formals)
                            new-sig)
                       body))))
       new-name))))

(defun %substitute-expression (expr descriptor new-name context)
  (smt:update-expression
   (a:curry #'%do-descriptor-name-update descriptor new-name)
   expr
   :context context
   :filter #'(lambda (expr ctx)
               (declare (ignore ctx))
               (smt:is-application? expr descriptor))))

(defmethod semgus:verifier-for-specification ((spec spec:relational-specification)
                                              semgus-problem
                                              &key produce-cex)
  (declare (ignore spec semgus-problem))
  (if produce-cex
      (call-next-method) ; We can't handle counter-examples with generic relational
      *concretizing-verifier-instance*))

(defmethod semgus:verifier-for-specification ((spec spec:universal-specification)
                                              semgus-problem
                                              &key produce-cex)
  (declare (ignore spec semgus-problem produce-cex))
  *concretizing-verifier-instance*)

(defmethod semgus:verifier-for-specification ((spec spec:existential-specification)
                                              semgus-problem
                                              &key produce-cex)
  (declare (ignore spec semgus-problem produce-cex))
  *concretizing-verifier-instance*)

(defun %concretize-and-query (specification relation semantics program context
                              &key (produce-cex nil))
  (let ((rel relation)
        (defns nil))
    (loop for descriptor in (spec:descriptors specification)
          do (multiple-value-bind (new-defns new-name)
                 (concretize-program-atom program descriptor semantics context)
               (setf defns
                     (append defns new-defns))
               (setf rel
                     (%substitute-expression rel descriptor new-name smt:*smt*))))

    (destructuring-bind (result cex)
        (query-smt defns rel produce-cex)
      (values result cex))))

(defun %verify-without-cex (specification semgus-problem program)
  "Verifies PROGRAM against SPECIFICATION, without computing counter examples"
  (let* ((sem (semgus:semantics semgus-problem))
         (rel (smt:copy-node (spec:expression specification))))
    (assert (typep specification 'spec:relational-specification))
    (%concretize-and-query specification rel sem program
                           (semgus:context semgus-problem))))

(defmethod semgus:verify-program ((verifier concretizing-verifier)
                                  specification
                                  semgus-problem
                                  program
                                  &key produce-cex)
  "Verifies PROGRAM against SEMGUS-PROBLEM"
  (if produce-cex
      (%verify-with-cex specification semgus-problem program)
      (%verify-without-cex specification semgus-problem program)))

(defgeneric %build-input-cex-query (spec)
  (:documentation "Builds an SMT query for getting a counter-example input state"))

(defgeneric %build-output-cex-query (spec input-vars input-vals)
  (:documentation "Builds an SMT query for getting a counter-example output state"))

;;;
;;; Universal specifications
;;;
(defmethod %build-input-cex-query ((spec spec:universal-specification))
  "Builds a query to get a counter-example input state for SPEC"
  (smt::quantifier-expression "forall"
                              (u:ensure-list (spec:output-symbols spec))
                              (u:ensure-list (spec:output-sorts spec))
                              (smt:$iff
                               (spec:relation spec)
                               (spec:constraint spec))))

;; TODO: doesn't always work for non-deterministic programs
(defmethod %build-output-cex-query
    ((spec spec:universal-specification) input-ex input-val)
  "Builds a query to get a counter-example output state for SPEC"
  (smt::quantifier-expression
   "exists"
   (u:ensure-list (spec:input-symbols spec))
   (u:ensure-list (spec:input-sorts spec))
   (smt:$and
    (apply #'smt:$and
           (map 'list #'smt:$= input-ex input-val))
    (spec:constraint spec))))

;;;
;;; Existential specifications
;;;
(defmethod %build-input-cex-query ((spec spec:existential-specification))
  "Builds a query for an existential specification."
  (smt::quantifier-expression "exists"
                              (u:ensure-list (spec:output-symbols spec))
                              (u:ensure-list (spec:output-sorts spec))
                              (smt:$and
                               (spec:relation spec)
                               (spec:constraint spec))))

(defmethod %build-output-cex-query
    ((spec spec:existential-specification) input-ex input-val)
  "Builds a query for getting output values for existential specifications"
  (smt::quantifier-expression
   "exists"
   (u:ensure-list (spec:input-symbols spec))
   (u:ensure-list (spec:input-sorts spec))
   (smt:$and
    (apply #'smt:$and
           (map 'list #'smt:$= input-ex input-val))
    (spec:constraint spec))))

;;;
;;; Counter-example generation
;;;
(defun %verify-with-cex (spec semgus-problem program)
  ;; Universal specifications: bind only the output variables and query
  (let ((to-query (%build-input-cex-query spec)))
    (multiple-value-bind (result cex)
        (%concretize-and-query spec
                               to-query
                               (semgus:semantics semgus-problem)
                               program
                               (semgus:context semgus-problem)
                               :produce-cex t)
      (case result
        (:unknown :unknown)
        (:valid :valid)
        (:invalid
         (let ((input-ex nil) (input-val nil))
           (loop for (var . value) in cex
                 for name = (smt:name var) ; VAR is an SMT constant
                 when (find name (spec:input-symbols spec)) do
                   (push var input-ex)
                   (push value input-val))

           (let ((to-query (%build-output-cex-query spec input-ex input-val)))
             (smt:with-solver* (solver smt:*cvc5*)
               (smt:with-scope (solver)
                 (smt:declare-constants solver to-query)
                 (smt:add-assertion solver to-query)
                 (let ((qres (smt:check-sat solver)))
                   (case qres
                     (:unknown :unknown)
                     (:unsat (error "Unsat on CEX generation!"))
                     (:sat
                      (let ((model (smt:get-model solver))
                            (ex-return))
                        ;; Merge input/output examples
                        (loop for ivar in input-ex
                              for ival in input-val
                              do (push (cons ivar ival) ex-return))
                        (loop for (var . value) in model
                              for name = (smt:name var)
                              when (find name (spec:output-symbols spec)) do
                                (push (cons var value) ex-return))
                        (values :invalid ex-return))))))))))))))
