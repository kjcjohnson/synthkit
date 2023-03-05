;;;;
;;;; reader for SemGuS problems in declarative s-expression format
;;;;
;;;; (note: this is not the SemGuS format. This is the sexpr parser output format)
(in-package #:com.kjcjohnson.synthkit.semgus)

(defvar *semgus-context* nil "Information about problem being parsed")

(defclass semgus-context ()
  ((grammar :accessor grammar
            :initarg :grammar)
   (chcs :accessor chcs
         :initarg :chcs)
   (head-relations :accessor head-relations
                   :initarg :head-relations)
   (constraints :accessor constraints
                :initarg :constraints)
   (root-relations :accessor root-relations
                   :initarg :root-relations)
   (term-name :accessor term-name
              :initarg :term-name)
   (term-type :accessor term-type
              :initarg :term-type)
   (auxiliary-functions :accessor auxiliary-functions
                        :initarg :auxiliary-functions))
  (:default-initargs
   :chcs nil
   :head-relations nil
   :constraints nil
   :auxiliary-functions nil))

(defclass semgus-chc ()
  ((head :accessor head :initarg :head)
   (body :accessor body :initarg :body)
   (constraint :accessor constraint :initarg :constraint)
   (input-variables :accessor input-variables :initarg :input-variables)
   (output-variables :accessor output-variables :initarg :output-variables)
   (variables :accessor variables :initarg :variables)
   (symbol-table :accessor symbol-table :initarg :symbol-table)
   (constructor :accessor constructor :initarg :constructor)))

(defclass semgus-chc-head ()
  ((name :accessor name :initarg :name)
   (argument-sorts :accessor argument-sorts :initarg :argument-sorts)
   (term-type :accessor term-type :initarg :term-type)
   (term-index :accessor term-index :initarg :term-index)
   (input-indexes :accessor input-indexes :initarg :input-indexes)
   (output-indexes :accessor output-indexes :initarg :output-indexes)
   (term-name :accessor term-name :initarg :term-name)
   (input-names :accessor input-names :initarg :input-names)
   (output-names :accessor output-names :initarg :output-names)))

(defclass semgus-chc-constructor ()
  ((name :accessor name :initarg :name)
   (arguments :accessor arguments :initarg :arguments)
   (argument-sorts :accessor argument-sorts :initarg :argument-sorts)
   (return-sort :accessor return-sort :initarg :return-sort)))

(defclass semgus-relation ()
  ((name :accessor name :initarg :name)
   (signature :accessor signature :initarg :signature)
   (arguments :accessor arguments :initarg :arguments)))

(defun production-for-chc (chc grammar)
  "Gets the production associated with the CHC in the grammar"
  (find (name (constructor chc))
        (g::productions grammar)
        :test (lambda (name prod)
                ;;(format t "~S : ~S~%" name (g:name (g:operator prod)))
                (eql name (g:name (g:operator prod))))))

(defun load-semgus-problem (filename)
  "Loads a SemGuS problem from the given file."
  (smt:init-smt)

  (let ((*semgus-context* (make-instance 'semgus-context)))
    (with-open-file (stream filename)
      (loop for sexpr = (let ((*package* (find-package "COM.KJCJOHNSON.SYNTHKIT.SEMGUS.READER.USER")))
                          (read stream nil :eof))
            until (eql sexpr :eof)
            doing (eval sexpr)))
    (multiple-value-bind (op-fn desc-map)
        (operationalize-semantics)
      (process-chcs-for-relational-semantics *semgus-context*)
      (make-instance 'semgus-problem
                     :specification (constraints-to-pbe)
                     :semantics (make-instance 'default-semantics
                                               :operational op-fn
                                               :descriptor-map desc-map
                                               :relational nil
                                               :relation-definitions nil)
                     :grammar (grammar *semgus-context*)
                     :context *semgus-context*))))

(defun operationalize-semantics ()
  "Operationalizes semantics - or, at least, tries to."
  (let ((opsem (make-hash-table))
        (desc-map (make-hash-table)))
    (loop for chc in (chcs *semgus-context*)
          for prod = (production-for-chc chc (grammar *semgus-context*))
          for descriptor = (name (head chc))
          for subtable = (gethash descriptor opsem (make-hash-table))
          doing
             (if (null prod)
                 (warn "No production in grammar for CHC with operator: ~a"
                       (name (constructor chc)))
                 (push
                  (operationalize-chc chc)
                  (gethash (g:operator prod) subtable)))
          unless (null prod)
            do (pushnew descriptor
                        (gethash (g:term-type (g:instance prod)) desc-map))
          end
          do (setf (gethash descriptor opsem) subtable))

    (values
     #'(lambda (descriptor prod)
         (if (null (g:name prod))
             ;; Special case: NT-to-NT productions
             (list (make-instance 'ast:calling-card
                                  :builder-function
                                  #'(lambda (sem-fns node node-children)
                                      (declare (ignore node node-children))
                                      (first sem-fns)) ;; Just return the child fn
                                  :descriptor-requests
                                  (list
                                   (make-instance 'ast:semantics-descriptor-request
                                                  :descriptor descriptor
                                                  :node-id 0))))
             (let ((subtable (gethash descriptor opsem)))
               (unless subtable
                 (error "No semantics for descriptor: ~s" descriptor))
               (gethash (g:operator prod) subtable))))
     desc-map)))

(defun operationalize-chc (chc)
  "Creates a semantic function for a CHC. The result is a function that takes an
input state and semantic functions for each child term"
  (com.kjcjohnson.synthkit.semgus.operationalizer:operationalize-chc+ chc smt:*smt* *semgus-context*))

(defun constraints-to-pbe ()
  "Extracts a PBE specification from the constraints"
  (let ((spec (make-instance 'io-specification)))
    (loop for constraint in (constraints *semgus-context*)
          for exs = (constraint-to-pbe constraint)
          when (null exs) do
            (warn "Unable to convert all constraints to PBE constraints.")
          when (not (null exs)) do
            (let ((input-state (getf exs :inputs))
                  (output-state (getf exs :output))
                  (descriptor (getf exs :descriptor)))
              (add-example spec
                           (smt:evaluate-state input-state)
                           (smt:evaluate-state output-state)
                           descriptor
                           :rel-input input-state
                           :rel-output output-state)))
    spec))

(defun constraint-to-pbe (constraint)
  "Tries to extract a PBE specification from a constraint"
  (let* ((appl-name (if (typep constraint 'smt::expression)
                        (smt:name constraint)
                        nil))
         (root-rels (root-relations *semgus-context*))
         (sf-term (term-name *semgus-context*))
         (appl-root (find appl-name root-rels :test #'eql :key #'name)))
    (cond
      ;; Standard SemGuS-style PBE
      (appl-root
       ;; Check if we're applying to the synth-fun term
       (let ((termchild (nth (term-index appl-root) (smt:children constraint))))
         (when (and (typep termchild 'smt::expression)
                    (eql (smt:name termchild) sf-term))
           (let ((inputs (smt:make-state
                          (loop for ix in (input-indexes appl-root)
                                for name in (input-names appl-root)
                                collect name
                                collect (nth ix (smt:children constraint)))))
                 (output (smt:make-state
                          (loop for output-ix in (output-indexes appl-root)
                                for output-name in (output-names appl-root)
                                collecting
                                (cons output-name
                                      (elt (smt:children constraint)
                                           output-ix))))))
             (list :inputs inputs :output output :descriptor (name appl-root))))))

      ;; Existentially quantified from SyGuS conversion
      ((and (typep constraint 'smt::quantifier)
            (string= (smt:name constraint) "exists")
            (= 1 (length (smt::arguments constraint)))
            (= 1 (length (smt::children constraint)))
            (typep (first (smt::children constraint)) 'smt::expression)
            (eql (smt:name (first (smt::children constraint)))
                 (smt:ensure-identifier "and")))
       (let ((output-var (first (smt::arguments constraint)))
             (rel-appl (first (smt::children (first (smt::children constraint)))))
             (equality (second (smt::children (first (smt:children constraint))))))
         (let* ((pbe-info (constraint-to-pbe rel-appl))
                (inputs (getf pbe-info :inputs))
                (descriptor (getf pbe-info :descriptor)))
           (when (and (not (null inputs))
                      (typep equality 'smt::expression)
                      (eql (smt:name equality) (smt:ensure-identifier "=")))

             (let ((root-rel (find descriptor root-rels :test #'eql :key #'name)))
               (if (eql output-var (smt:name (first (smt:children equality))))
                   (list :descriptor descriptor
                         :inputs inputs
                         :output
                         (smt:make-state ; SyGuS will only have one output var
                          (list
                           (first (output-names root-rel))
                           (second (smt:children equality)))))
                   (list :descriptor descriptor
                         :inputs inputs
                         :output
                         (smt:make-state
                          (list
                           (first (output-names root-rel))
                           (first (smt:children equality)))))))))))


        (t
        nil))))