;;;
;;; cegis support for semgus
;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

;;
;; CEGIS specification - holds counter-examples and wraps the underlying formula
;;
(defclass cegis-specification ()
  ((counter-examples
    :initarg :counter-examples
    :initform (list)
    :accessor counter-examples)
   (formula
    :initarg :formula
    :reader formula)))

(defun ensure-cegis-problem (problem &key (classname 'cegis-specification))
  "Returns a problem with a CEGIS specification - a fresh one."
  (assert (subtypep classname 'cegis-specification) nil "Not a CEGIS spec: ~A" classname)
  (if (typep (specification problem) classname)
      (u:copy-instance problem
                       :specification (u:copy-instance (specification problem)))
      (u:copy-instance problem
                       :specification
                       (make-instance classname
                                      :formula (specification problem)))))

(defgeneric add-counter-example-for-specification (problem c-ex cegis-spec formula)
  (:documentation
   "Adds a counter-example model to the given CEGIS spec, with the given formula.")
  (:method (problem c-ex (cegis-spec cegis-specification) formula)
    (declare (ignore formula))
    (push c-ex (counter-examples cegis-spec))))

(defun add-counter-example (problem c-ex)
  "Adds the given counter-example (a-list of variable/values) to the problem specification."
  (let ((cegis-spec (the cegis-specification (specification problem))))
    (add-counter-example-for-specification problem c-ex cegis-spec (formula cegis-spec))))

;;
;; Finding counter-examples. Returns a model as an alist.
;;
(defgeneric find-counter-example-for-specification (program problem cegis-spec formula)
  (:documentation
   "Finds a counter-example model (alist) for the given program, or NIL if none exist."))

(defmethod find-counter-example-for-specification (program
                                                   problem
                                                   cegis-spec
                                                   (spec formula-specification))
  (smt:with-solver (solver smt::*cvc4*)
    (smt:dump-commands solver
                        (ast::as-smt-query program
                                           (semantics problem)
                                           (relation-name spec)))
    ; (format *trace-output* "; NEG: ~a~%" (smt:not (formula spec)))
    (smt:declare-constants solver (formula spec))
    (smt:add solver (smt:$not (formula spec)))
    ;(smt:add solver can we negate the formula somehow?
    (let ((q-res (smt:check-sat solver)))
      (cond
        ((eql :unknown q-res) (error "Unknown result!"))
        ((eql :unsat q-res) nil)
        ((eql :sat q-res) (smt:get-model solver))
        (t (error "Invalid SMT response: ~s" q-res))))))

(defun find-counter-example (program problem)
  "Finds a counter-example for the given candidate program."
  (let ((cegis-spec (the cegis-specification (specification problem))))
    (find-counter-example-for-specification program problem cegis-spec (formula cegis-spec))))

;;;
;;;  ============= NEW ============
;;;
(defgeneric cegis-supported-for-specification? (specification semgus-problem)
  (:documentation "Checks if CEGIS is supported for SPECIFICATION")
  (:method (spec problem)
    (declare (ignore spec problem))
    nil)
  (:method ((spec spec:universal-specification) problem)
    (declare (ignore problem))
    t)
  (:method ((spec spec:existential-specification) problem)
    (declare (ignore problem))
    t)
  (:method ((spec spec:cegis-specification) problem)
    (cegis-supported-for-specification? (spec:relational spec) problem)))

(defun cegis-supported? (semgus-problem)
  "Checks if SEMGUS-PROBLEM is eligible for CEGIS"
  (cegis-supported-for-specification? (specification semgus-problem) semgus-problem))

(defun cegis-wrapper (semgus-problem synth-fun)
  "Does CEGIS. SYNTH-FUN should be a function taking a semgus problem as the only arg."
  (assert (cegis-supported? semgus-problem))

  (let* ((cegis-spec (spec:convert-to-cegis (specification semgus-problem)))
         (spec (spec:relational-specification cegis-spec)))
    ;; TODO: maybe we should enumerate the smallest program first, instead
    ;;       of passing an empty intersection-specification and save a step?
    (loop with verifier = (verifier-for-specification spec semgus-problem
                                                      :produce-cex t)
          for new-problem = (replace-specification semgus-problem
                                                   (spec:cegis-examples cegis-spec))
          for candidate = (funcall synth-fun new-problem)
          when (listp candidate) do
            (setf candidate (first candidate))
          end
          do (multiple-value-bind (result cex)
                 (verify-program verifier spec semgus-problem candidate :produce-cex t)
               (when (eql result :valid)
                 (return-from cegis-wrapper candidate))
               (when (eql result :unknown)
                 (error "Unknown CEGIS verification!"))
               ;; Convert to state
               (let ((rootrel (lookup-root (first (spec:descriptors spec))
                                           (context semgus-problem)))
                     (input-vars)
                     (output-vars))
                 (loop for formal across (chc:formals rootrel)
                       for role across (chc:roles rootrel)
                       for actual in (smt:children (spec:relation spec))
                       when (eql role :input) do
                         (push (cons formal (cdr (assoc (smt:name actual)
                                                        cex
                                                        :key #'smt:name)))
                               input-vars)
                       end
                       when (eql role :output) do
                         (push (cons formal (cdr (assoc (smt:name actual)
                                                        cex
                                                        :key #'smt:name)))
                               output-vars)
                       end)
                 (flet ((pp-varlist (varlist)
                          (format nil "[~{~a~^, ~}]"
                                  (map 'list
                                       #'(lambda (x)
                                           (format nil "~a: ~a"
                                                   (smt:identifier-string (car x))
                                                   (cdr x)))
                                       varlist))))
                   (format *trace-output*
                           "~&~%------------- CEGIS -------------
input ---> ~a
output --> ~a
---------------------------------~%~%"
                           (pp-varlist input-vars)
                           (pp-varlist output-vars)))
                 (spec:add-example
                  cegis-spec
                  (first (spec:descriptors spec))
                  (smt:make-state input-vars)
                  (smt:make-state output-vars)))))))
