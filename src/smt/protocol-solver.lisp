;;;;
;;;; Protocol for SMT solvers
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defclass solver ()
  ()
  (:documentation "Base class for SMT solvers"))

(defgeneric make-solver (solver-config)
  (:documentation "Makes a solver from a configuration"))

(defgeneric initialize-solver (solver-class config)
  (:documentation "Initializes a solver from a configuration"))

(defgeneric finalize-solver (solver)
  (:documentation "Finalizes a solver and closes all associated resources"))

(defgeneric cleanup-solver (solver)
  (:documentation "Completely closes a solver connection"))

;;;
;;; Raw
;;;
(defgeneric dump (solver string)
  (:documentation "Dumps raw commands in STRING to SOLVER"))

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
;;; Options and initializations
;;;
(defgeneric reset-solver (solver)
  (:documentation "Requests that SOLVER executes a `reset` command"))

(defgeneric set-logic (solver logic)
  (:documentation "Sets the solver logic to the string LOGIC. If this is not the first
command of a session, the solver may be reset first!"))

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

(defgeneric read-model (solver)
  (:documentation "Reads a model from the solver (without calling `get-model`)"))

(defgeneric get-model (solver)
  (:documentation "Gets a model after calling CHECK-SAT"))
