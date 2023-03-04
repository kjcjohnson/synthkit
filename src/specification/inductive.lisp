;;;;
;;;; Inductive specifications - predicates over input and output states
;;;;
(in-package #:com.kjcjohnson.synthkit.specification)

(defclass inductive-specification (specification)
  ((input-state :initarg :input-state
                :reader input-state
                :documentation "Concrete input state for this specification")
   (descriptor :initarg :descriptor
               :reader descriptor
               :documentation "Semantics descriptor for this specification")
   (predicate :initarg :predicate
              :reader predicate
              :documentation "Function over two arguments taking the input and output
state to test for satisfication. Returns T if satisfied, NIL otherwise."))
  (:documentation "A specification over a concrete input and any output states"))

(defclass io-specification (inductive-specification)
  ((output-state :initarg :output-state
                 :reader output-state
                 :documentation "Concrete output state for this specification"))
  (:default-initargs :predicate #'smt:state=)
  (:documentation "A specification over a concrete input and concrete output state"))

(defun is-only-inductive? (spec)
  "Checks if SPEC only includes inductive specifications"
  (is-only? spec 'inductive-specification))

(defun is-only-io? (spec)
  "Checks if SPEC only includes IO specifications"
  (is-only? spec 'io-specification))
