;;;;
;;;; Base specification class
;;;;
(in-package #:com.kjcjohnson.synthkit.specification)

(defclass specification ()
  ()
  (:documentation "A generic specification for a problem"))

(defclass relational-specification (specification)
  ((expression :initarg :expression
               :reader expression
               :documentation "SMT expression for this specification")
   (descriptors :initarg :descriptors
                :reader descriptors
                :documentation "Descriptors used in this specification"))
  (:documentation "A specification from an SMT expression"))
