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
         :documentation "The name of this head (as an SMT symbol). This name is also
the descriptor for this head.")
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

(defun filter-role (role data roles)
  "Filters elements from DATA where the matching element in ROLES matches ROLE. Both
ROLES and DATA should be sequences of the same length. Returns a list."
  (assert (= (length data) (length roles)))
  (loop for datum across data
        for rolex across roles
        when (eql role rolex) collect datum))

(defun role-indices (role head)
  "Gets indices from HEAD's signature with the given role"
  (loop for r across (roles head)
        for i from 0
        when (eql role r) collect i))

(defun term-index (head)
  "Gets the term index for the head"
  (declare (type head head))
  (position :term (roles head)))

(defun term-type (head)
  "Gets the term type for the head"
  (check-type head head)
  (let ((term-ix (term-index head)))
    (elt (signature head) term-ix)))

(defun term-name (head)
  "Gets the term name for the head"
  (check-type head head)
  (let ((term-ix (term-index head)))
    (elt (formals head) term-ix)))

(defun input-indices (head)
  "Gets indices of input formals in HEAD"
  (role-indices :input head))

(defun input-formals (head)
  "Gets formals that are inputs in HEAD"
  (loop for i in (role-indices :input head)
        collecting (elt (formals head) i)))

(defun output-indices (head)
  "Gets indices of output formals in HEAD"
  (role-indices :output head))

(defun output-formals (head)
  "Gets formals that are outputs in HEAD"
  (loop for i in (role-indices :output head)
        collecting (elt (formals head) i)))

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

(defun is-forward-declared-head? (obj)
  "Checks if OBJ is a forward declared head or no"
  (typep obj 'forward-declared-head))

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

;;; Convenience readers for relations
(defmethod name ((relation relation)) (name (head relation)))
(defmethod signature ((relation relation)) (signature (head relation)))

(defun input-actuals (relation)
  "Returns a list of input actuals of RELATION"
  (loop for ii in (input-indices (head relation))
        for input = (aref (actuals relation) ii)
        collecting input))

(defun output-actuals (relation)
  "Returns a list of output actuals of RELATION"
  (loop for oi in (output-indices (head relation))
        for output = (aref (actuals relation) oi)
        collecting output))

(defun term-actual (relation)
  "Returns the actual term variable name of RELATION"
  (aref (actuals relation) (term-index (head relation))))

(defclass chc ()
  ((symbols :reader symbol-table
            :initarg :symbol-table
            :type symbol-table
            :documentation "Symbols for this CHC")
   (head :reader head
         :initarg :head
         :type head
         :documentation "The head associated with this CHC")
   (body :reader body
         :initarg :body
         :type list ; of relation
         :documentation "Child semantic relations used in this CHC")
   (constraint :reader constraint
               :initarg :constraint
               :type smt:term
               :documentation "SMT expression for this CHC's constraint")
   (constructor :reader constructor
                :initarg :constructor
                :type constructor
                :documentation "Constructor for this CHC")
   (id :reader id
       :initarg :id
       :type symbol
       :documentation "A unique identifier for this CHC")
   (data :reader data
         :initarg :data
         :type hash-table
         :documentation "Arbitrary map of auxiliary data for this CHC"))
  (:default-initargs :data (make-hash-table))
  (:documentation "A CHC."))

(defun get-data (key chc &optional default)
  "Gets a piece of CHC extra data"
  (gethash key (data chc) default))

(defun (setf get-data) (value key chc)
  "Sets a piece of CHC extra data"
  (setf (gethash key (data chc)) value))
