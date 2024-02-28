;;;;
;;;; JSON events definitions
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.writer)

;;;
;;; Clean up some JSON naming mess with the MOP
;;;
(defclass json-meta-class (standard-class) ())
(defclass json-nameable-direct-slot (mop:standard-direct-slot-definition)
  ((json-name :initarg :json-name
              :documentation "Alternative JSON name for this slot")
   (json-transformer :initarg :json-transformer
                     :documentation "Function for transforming JSON value")))
(defclass json-nameable-effective-slot (mop:standard-effective-slot-definition)
  ((json-name :initarg :json-name
              :documentation "Alternative JSON name for this slot")
   (json-transformer :initarg :json-transformer
                     :documentation "Function for transforming JSON value")))
(defmethod mop:compute-effective-slot-definition ((class json-meta-class) name dslots)
  (let ((computed (call-next-method))
        (direct (find name dslots :key #'mop:slot-definition-name)))
    (when (slot-boundp direct 'json-name)
      (setf (slot-value computed 'json-name)
            (slot-value direct 'json-name)))
    (when (slot-boundp direct 'json-transformer)
      (setf (slot-value computed 'json-transformer)
            (slot-value direct 'json-transformer)))
    computed))
(defmethod mop:direct-slot-definition-class ((class json-meta-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'json-nameable-direct-slot))
(defmethod mop:effective-slot-definition-class ((class json-meta-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'json-nameable-effective-slot))
(defmethod mop:validate-superclass ((class json-meta-class) (superclass standard-class))
  (declare (ignore class superclass))
  t)

;;;
;;; Base event
;;;
(defclass json-event ()
  ((event :reader event-name
          :initarg :event
          :json-name "$event"
          :type string)
   (type :reader event-type
         :initarg :type
         :json-name "$type"
         :type string))
  (:metaclass json-meta-class)
  (:documentation "The superclass for all JSON events"))

(defmethod jzon:coerced-fields ((event json-event))
  "Adds in the overall JSON fields"
  (let ((base-fields (and (next-method-p) (call-next-method))))
    (loop for entry in base-fields
          for slot-name = (car entry)
          for slot = (find slot-name (mop:class-slots (class-of event))
                           :key #'mop:slot-definition-name)
          if (slot-boundp slot 'json-name)
            do (setf (car entry) (slot-value slot 'json-name))
          if (slot-boundp slot 'json-transformer)
            do (setf (second entry)
                     (funcall (slot-value slot 'json-transformer) (second entry))))
    base-fields))

(defmacro define-event (name super event &rest defns)
  "Defines an event"
  (declare (type (or null string) event))
  `(defclass ,name ,super
     ,@defns
     ,@(unless (null event) `((:default-initargs :event ,event)))
     (:metaclass json-meta-class)))

;;;
;;; Type superclasses
;;;

(define-event semgus-event (json-event) nil
  ()
  (:default-initargs :type "semgus")
  (:documentation "Superclass for semgus-type events"))

(define-event meta-event (json-event) nil
  ()
  (:default-initargs :type "meta")
  (:documentation "Superclass for meta-type events"))

(define-event smt-event (json-event) nil
  ()
  (:default-initargs :type "smt")
  (:documentation "Superclass for smt-type events"))

;;;
;;; Event implementations
;;;

(define-event declare-term-type-event (semgus-event) "declare-term-type"
  ((name :reader term-name
         :initarg :name
         :type semgus:term-type))
  (:documentation "Event for declaring a term type's existence"))

(define-event define-term-type-event (semgus-event) "define-term-type"
  ((name :reader term-name
         :initarg :name
         :type semgus:term-type)
   (constructors :reader constructors
                 :initarg :constructors
                 :type list))
  (:documentation "Event for defining a term type's constructors"))

(defmethod initialize-instance :after ((ev define-term-type-event) &key name)
  "Sets the constructors for the term type definition event"
  (u:set-slot-if-unbound ev 'constructors (semgus:term-type-constructors name))))

(define-event chc-event (semgus-event) "chc"
  ((id :reader chc-id
       :initarg :id)
   (head :reader chc-head
         :initarg :head
         :type chc:head)
   (body :reader chc-body
         :initarg :body
         :json-name "bodyRelations"
         :type list)
   (constraint :reader chc-constraint
               :initarg :constraint)
   (constructor :reader chc-constructor
                :initarg :constructor)
   (symbols :reader chc-symbols
            :initarg :symbols)))

(defmethod initialize-instance :after ((ev chc-event) &key chc)
  "Sets up the CHC event data from a single CHC parameter"
  (u:set-slot-if-unbound ev 'head (chc:head chc))
  (u:set-slot-if-unbound ev 'body (chc:body chc))
  (u:set-slot-if-unbound ev 'id (smt:identifier-smt (chc:id chc)))
  (u:set-slot-if-unbound ev 'constraint (chc:constraint chc))
  (u:set-slot-if-unbound ev 'constructor (chc:constructor chc))
  (u:set-slot-if-unbound ev 'symbols (chc:symbol-table chc)))

(define-event synth-fun-event (semgus-event) "synth-fun"
  ((name :initarg :name
         :json-transformer smt:identifier-smt)
   (term-type :initarg :term-type
              :json-name "termType")
   (grammar :initarg :grammar))
  (:documentation "The synth-fun event with grammar"))

(define-event constraint-event (semgus-event) "constraint"
  ((constraint :initarg :constraint))
  (:documentation "A synthesis constraint"))

(define-event check-synth-event (semgus-event) "check-synth"
  ()
  (:documentation "Event for the check-synth command"))

(define-event set-info-event (meta-event) "set-info"
  ((keyword :reader info-keyword
            :initarg :keyword
            :type string)
   (value :reader info-value
          :initarg :value)))

(define-event end-of-stream-event (meta-event) "end-of-stream"
  ()
  (:documentation "Event for the end of the JSON stream"))
