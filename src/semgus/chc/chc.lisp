;;;;
;;;; CHCs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.chc)

(defclass constructor ()
  ((name :accessor name :initarg :name)
   (arguments :accessor arguments :initarg :arguments)
   (argument-sorts :accessor argument-sorts :initarg :argument-sorts)
   (return-sort :accessor return-sort :initarg :return-sort)))

(defclass head ()
  ((name :reader name
         :initarg :name
         :type symbol
         :documentation "The name of this head (as an SMT symbol). This name is also the
descriptor for this head.")
   (signature :reader signature
              :initarg :signature
              :type (vector symbol)
              :documentation "Vector of sorts of the relation")
   (roles :reader roles
          :initarg :roles
          :type (vector (member :input :output :term :unknown))
          :documentation "The role of each formal in the signature")
   (formals :reader formals
            :initarg :formals
            :type (vector symbol)
            :documentation "The names associated with each parameter. NOTE: this slot
assumes that every CHC head will have the same named parameters in the same location.
This is not necessarily true, but it is a consequence of how we encode SemGuS problems
in SMT-LIB2 files. We're likely to remove this assumption in the future, and with it,
this slot (and data). You have been warned."))
  (:documentation "A CHC head"))

(defclass forward-declared-head ()
  ((name :reader name
         :initarg :name
         :type symbol
         :documentation "A forward reference to a CHC head to be resolved later.")
   (signature :reader signature
              :initarg :signature
              :type (vector symbol)
              :documentation "Vector of sorts of the relation"))
  (:documentation "A placeholder for a CHC head that has not yet been seen."))

(defclass relation ()
  ((head :reader head
         :initarg :head
         :type (or head forward-declared-head)
         :documentation "The CHC head associated with this relation.")
   (actuals :reader actuals
            :initarg :actuals
            :type (vector symbol)
            :documentation "The actual symbols passed as arguments to this relation"))
  (:documentation "A call to a semantic relation, including the head and arguments"))

(defclass chc ()
  ((symbols :reader symbol-table
            :initarg :symbols
            :type symbol-table
            :documentation "Symbols for this CHC")
   (head :reader head
         :initarg :head
         :type head
         :documentation "The head associated with this CHC")
   (body :reader body
         :initarg :body
         :type (vector relation)
         :documentation "Child semantic relations used in this CHC")
   (constraint :reader constraint
               :initarg :constraint
               :type smt::expression ; TODO: do we actually want EXPRESSION exported?
               :documentation "SMT expression for this CHC's constraint")
   (constructor :reader constructor
                :initarg :constructor
                :type constructor
                :documentation "Constructor for this CHC"))
  (:documentation "A CHC."))
