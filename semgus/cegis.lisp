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
    (smt:add solver (smt:not (formula spec)))
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
