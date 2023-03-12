;;;;
;;;; Handling for SMT datatypes
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defvar *no-canonical-datatypes* nil "Feature flag for disabling the creation of
canonical datatype instances. This might be useful if the problem being solved churns
through a bunch of unique datatype instances, and the extra cost to compare datatypes
(and states) is worth the memory savings. Note that setting this flag also affects
state comparisons - states with non-canonical datatype instances also cannot be
converted to canonical state instances!")

(defclass datatype ()
  ((name
    :initarg :name
    :reader name
    :documentation "The name of this datatype (as an SMT identifier)")
   (constructors
    :initarg :constructors
    :reader constructors
    :documentation "List of constructors that make up this datatype"))
  (:default-initargs :constructors nil)
  (:documentation "Defines classes of datatypes"))

(defclass datatype-constructor ()
  ((name
    :initarg :name
    :reader name
    :documentation "The name of this constructor (as an SMT identifier)")
   (children
    :initarg :children
    :reader children
    :documentation "List of sorts of this constructor's children")
   (canonical-instances
    :initform (trivial-garbage:make-weak-hash-table :test #'equal :weakness :value)
    :reader canonical-instances
    :documentation "Table of canonical instances for hash-consing"))
  (:documentation "Defines constructors associated with a datatype"))

(defclass datatype-instance ()
  ((datatype
    :initarg :datatype
    :reader datatype
    :documentation "Datatype that this instance is an instantiation of")
   (constructor
    :initarg :constructor
    :reader constructor
    :documentation "Constructor that this instance is an instantiation of")
   (children
    :initarg :children
    :reader children
    :documentation "Child values of this instance")
   (canonical
    :initarg :canonical
    :reader is-canonical-instance?
    :documentation "Signals if this instance is a canonical instance"))
  (:default-initargs :canonical nil)
  (:documentation "A concrete instance of a datatype/constructor"))

(defun is-datatype-instance? (object)
  "Checks if OBJECT is a datatype instance or no"
  (typep object 'datatype-instance))

(defun add-datatype-constructor (datatype constructor)
  "Adds a constructor to the given datatype."
  (declare (type datatype datatype)
           (type datatype-constructor constructor))
  (push constructor (slot-value datatype 'constructors)))

(defun make-datatype-instance (datatype constructor children)
  "Creates an instance of a datatype."
  (declare (type datatype datatype)
           (type datatype-constructor constructor))
  (if *no-canonical-datatypes*
      (make-instance 'datatype-instance
                     :datatype datatype
                     :constructor constructor
                     :children children
                     :canonical nil)

      (let* ((canonical-instance-table (canonical-instances constructor))
             (canonical-instance (gethash children canonical-instance-table)))
        (when (null canonical-instance)
          (setf children (copy-list children))
          (setf canonical-instance (make-instance 'datatype-instance
                                                  :datatype datatype
                                                  :constructor constructor
                                                  :children children
                                                  :canonical t))
          (setf (gethash children canonical-instance-table) canonical-instance))
        canonical-instance)))

(defun datatype= (instance1 instance2)
  "Compares two datatype instances."
  (declare (type datatype-instance instance1 instance2))

  (when (and (is-canonical-instance? instance1)
             (is-canonical-instance? instance2))
    (return-from datatype= (eql instance1 instance2)))

  (unless *no-canonical-datatypes*
    (warn "Comparing non-canonical datatype instances"))
  (and (eql (datatype instance1) (datatype instance2))
       (eql (constructor instance1) (constructor instance2))
       (every #'core-= (children instance1) (children instance2))))

(defmethod print-object ((dt datatype) stream)
  "Gets a printed representation of a datatype."
  (print-unreadable-object (dt stream)
    (format stream "DT:~a" (name dt))))

(defmethod print-object ((dtc datatype-constructor) stream)
  "Gets a printed representation of a datatype constructor."
  (print-unreadable-object (dtc stream)
    (format stream "DTC:~a" (name dtc))))

(defmethod print-object ((dti datatype-instance) stream)
  "Gets a printed representation of a datatype instance."
  (print-unreadable-object (dti stream)
    (format stream
            "DTI:~a/~a: ~s"
            (name (datatype dti))
            (name (constructor dti))
            (children dti))))

(defmethod com.kjcjohnson.kale:equals
    ((d1 datatype-instance) (d2 datatype-instance))
  (warn "Computing datatype equality")
  (datatype= d1 d2))

(defmethod com.kjcjohnson.kale:get-hash-code ((dti datatype-instance))
  "Computes a hash code for DTI."
  (warn "Computing datatype hash code")
  (logxor (sxhash (datatype dti))
          (sxhash (constructor dti))
          (reduce #'logxor
                  (loop for child in (children dti)
                        collecting (com.kjcjohnson.kale:get-hash-code child)))))
