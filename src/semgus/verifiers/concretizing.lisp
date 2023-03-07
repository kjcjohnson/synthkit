;;;;
;;;; A simple concretizing verifier - can handle loop-free programs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.verifiers)

(defclass concretizing-verifier () ())

(defun query-smt (body-defns constraint produce-cex)
  (smt:with-solver (solver smt::*cvc4*)
    (smt:dump-commands solver body-defns)

    (smt:declare-constants solver constraint)
    (smt:add solver (smt:$not constraint))

    (let ((q-res (smt:check-sat solver)))
      (cond
        ((eql :unknown q-res)
         (list :unknown nil))
        ((eql :unsat q-res)
         (list :invalid nil))
        ((eql :sat q-res)
         (list :valid (and produce-cex (smt:get-model solver))))
        (t (error "Invalid SMT response: ~s" q-res))))))

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

(defun concretize-node-chc (node chc semantics)
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
                   (concretize-program-node child-node (chc:name body-rel) semantics)
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

(defun concretize-program-node (node descriptor semantics)
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
                                   (concretize-node-chc node chc semantics))
                          doing (setf child-defns (append child-defns defns))
                          collecting chc-expr)))
    ;; Fixup for multiple CHCs
    (let ((body
            (if (= 1 (length chc-exprs))
                (first chc-exprs)
                (apply #'smt:$or chc-exprs))))

      ;; Write function definition
      (let ((term-ix (chc:term-index (chc:head (first chcs))))
            (new-sig nil)
            (new-formals nil))
        (loop for i from 0 below (length (chc:formals (chc:head (first chcs))))
              unless (= i term-ix)
                collect (elt (chc:formals (chc:head (first chcs))) i) into new-f
                and
                  collect (elt (chc:signature (chc:head (first chcs))) i) into new-s
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
           new-name))))))

(defun %substitute-expression (expr descriptor new-name context)
  (smt:update-expression
   (a:curry #'%do-descriptor-name-update descriptor new-name)
   expr
   :context context
   :filter #'(lambda (expr ctx)
               (declare (ignore ctx))
               (smt:is-application? expr descriptor))))

(defmethod semgus:verify-program ((verifier concretizing-verifier)
                                  semgus-problem
                                  program
                                  &key produce-cex)
  "Verifies PROGRAM against SEMGUS-PROBLEM"
  (let* ((spec (semgus:specification semgus-problem))
         (sem (semgus:semantics semgus-problem))
         (rel (smt:copy-node (spec:expression spec)))
         (defns nil))
    (assert (typep spec 'spec:relational-specification))

    ;; For universal formulas: strip off the quantifier
    ;; TODO: actually handle this robustly...and support other formula formats
    (when (and (typep rel 'smt::quantifier)
               (string= (smt:name rel) "forall"))
      (setf rel (first (smt:children rel))))

    (loop for descriptor in (spec:descriptors spec)
          do (multiple-value-bind (new-defns new-name)
                 (concretize-program-node program descriptor sem)
               (setf defns
                     (append defns new-defns))
               (setf rel
                     (%substitute-expression rel descriptor new-name smt:*smt*))))

    (destructuring-bind (result cex)
        (query-smt defns rel produce-cex)
      (values result cex))))
