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

;;;
;;; Event implementations
;;;

(defclass declare-term-type-event (semgus-event)
  ((name :reader term-name
         :initarg :name
         :type semgus:term-type))
  (:default-initargs :event "declare-term-type")
  (:documentation "Event for declaring a term type's existence"))

(defclass define-term-type-event (semgus-event)
  ((name :reader term-name
         :initarg :name
         :type semgus:term-type)
   (constructors :reader constructors
                 :initarg :constructors
                 :type list))
  (:default-initargs :event "define-term-type")
  (:documentation "Event for defining a term type's constructors"))

(defmethod initialize-instance :after ((ev define-term-type-event) &key name)
  "Sets the constructors for the term type definition event"
  (unless (slot-boundp ev 'constructors)
    (setf (slot-value ev 'constructors) (semgus:term-type-constructors name))))

(defclass chc-event (semgus-event)
  ((head :reader chc-head
         :initarg :head
         :type chc:head)
   (body :reader chc-body
         :initarg :body
         :type list))
  (:default-initargs :event "chc"))

(defclass check-synth-event (semgus-event)
  ()
  (:default-initargs :event "check-synth")
  (:documentation "Event for the check-synth command"))

(defclass set-info-event (meta-event)
  ((keyword :reader info-keyword
            :initarg :keyword
            :type string)
   (value :reader info-value
          :initarg :value))
  (:default-initargs :event "set-info"))
