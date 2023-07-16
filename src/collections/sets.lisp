;;;;
;;;; Sets
;;;;
(in-package #:com.kjcjohnson.synthkit.collections.impl)

;;;
;;; Sets based on lists
;;;
(defclass list-set (c:set)
  ((set-list :accessor set-list
             :initarg :set-list
             :type list
             :documentation "The underlying list associated with this set")
   (test :accessor test
         :initarg :test
         :type function
         :documentation "Test function to use when comparing elements"))
  (:default-initargs :set-list nil)
  (:documentation "An implementation of sets using a list"))

(defmethod c:new ((descriptor (eql :list-set)) &key (test #'eql))
  "Creates a set based on a list"
  (make-instance 'list-set :test test))

(defmethod c:add ((set list-set) item)
  "Adds an item to SET if not already present"
  (pushnew item (set-list set) :test (test set))
  set)

(defmethod c:clear ((set list-set))
  "Removes all items from SET"
  (setf (set-list set) nil)
  set)

(defmethod c:contains ((set list-set) item)
  "Checks if SET contains ITEM"
  (*:true (find item (set-list set) :test (test set))))

(defmethod c:count ((set list-set))
  "Returns the number of items in SET"
  ;; N.B.: we should keep track of this count in a slot instead of recomputing it.
  (length (set-list set)))

(defmethod c:remove ((set list-set) item)
  "Removes ITEM from SET"
  (*:deletef (set-list set) item :test (test set))
  set)

(defmethod print-object ((set list-set) stream)
  "Prints SET to STREAM"
  (print-unreadable-object (set stream)
    (format stream "list-set ~a" (set-list set))))
