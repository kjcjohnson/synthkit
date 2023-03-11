;;;;
;;;; A CEGIS specification
;;;;
(in-package #:com.kjcjohnson.synthkit.specification)

(defclass cegis-specification (specification)
  ((relational :initarg :relational
               :reader relational-specification
               :documentation "Relational specification for CEGIS")
   (examples :initarg :examples
             :accessor cegis-examples
             :documentation "Examples generated during CEGIS"))
  (:default-initargs :examples (list))
  (:documentation "A specification for use during CEGIS"))

(defun convert-to-cegis (specification)
  "Converts SPECIFICATION into a CEGIS specification."
  (if (subtypep (type-of specification) 'cegis-specification)
      specification ; Already a CEGIS specification
      (make-instance 'cegis-specification :relational specification)))
;; TODO: we should pull out any IO examples in SPECIFICATION into the CEGIS spec
