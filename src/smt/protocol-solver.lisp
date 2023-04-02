;;;;
;;;; Protocol for SMT solvers
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defclass solver ()
  ()
  (:documentation "Base class for SMT solvers"))

(defgeneric initialize-solver (solver)
  (:documentation "Initializes a solver"))

(defgeneric finalize-solver (solver)
  (:documentation "Finalizes a solver and closes all associated resources"))

;;;
;;; Scopes
;;;
(defgeneric push-scope (solver &optional levels)
  (:documentation "Pushes an SMT scope, or LEVELS scopes if provided."))

(defgeneric pop-scope (solver &optional levels)
  (:documentation "Pops an SMT scope, or LEVELS scopes if provided."))

(defmacro with-scope ((solver &optional (levels 1)) &body body)
  "Pushes and pops an SMT scope, or LEVELS scopes if provided."
  (a:once-only (solver levels)
    `(unwind-protect
          (progn
            (push-scope ,solver ,levels)
            ,@body)
       (pop-scope ,solver ,levels))))

;;;
;;; Assertions and declarations
;;;
(defgeneric add-assertion (solver &rest assertions)
  (:documentation "Adds ASSERTIONS to SOLVER"))

(defgeneric declare-constant (solver &rest constants)
  (:documentation "Declares a constant. CONSTANTS is a list of SMT:CONSTANT objects."))

;;;
;;; Checking
;;;
(defgeneric check-sat (solver)
  (:documentation "Checks the current assertions for satisfibility"))

(defgeneric get-model (solver)
  (:documentation "Gets a model after calling CHECK-SAT"))
