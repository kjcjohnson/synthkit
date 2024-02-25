;;;;
;;;; JSON events definitions
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.writer)

(defclass json-event ()
  ((event :reader event-name
          :initarg :event
          :type string)
   (type :reader event-type
         :initarg :type
         :type string))
  (:documentation "The superclass for all JSON events"))

(defmethod jzon:coerced-fields ((event json-event))
  "Adds in the overall JSON fields"
  (let ((base-fields nil))
    (when (next-method-p)
      (setf base-fields
            (remove-if (*:rcurry #'member '(event type)) (call-next-method)
                       :key #'first)))
    (append (list
             (list "$name" (event-name event) 'string)
             (list "$type" (event-type event) 'string))
            base-fields)))

;;;
;;; Type superclasses
;;;

(defclass semgus-event (json-event)
  ()
  (:default-initargs :type "semgus")
  (:documentation "Superclass for semgus-type events"))

(defclass meta-event (json-event)
  ()
  (:default-initargs :type "meta")
  (:documentation "Superclass for meta-type events"))

(defclass smt-event (json-event)
  ()
  (:default-initargs :type "smt")
  (:documentation "Superclass for smt-type events"))
