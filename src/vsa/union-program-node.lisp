(in-package #:com.kjcjohnson.synthkit.vsa)

;;;
;;; Union program node enumerator
;;;
(defclass union-program-node-enumerator (program-node-enumerator)
  ((child-enumerator :accessor %child-enumerator
                     :initarg :child-enumerator
                     :type program-node-enumerator
                     :documentation "Enumerator for the current child")
   (program-list :accessor %program-list
                 :initarg :program-list)
   (next-pointer :accessor %next-pointer
                 :initarg :next-pointer)
   (index :accessor %index
          :initarg :index))
  (:default-initargs :index -1 :child-enumerator nil))

(defun union-program-node-reset (e)
  (setf (%next-pointer e) (%program-list e))
  (setf (%index e) -1))

(defun union-program-node-current (e)
  (current (%child-enumerator e)))

(defun union-program-node-move-next (e)
  (flet ((advance-children ()
           (incf (%index e))
           (let ((next-node (pop (%next-pointer e))))
             (if (null next-node)
                 nil
                 (progn
                   (setf (%child-enumerator e) (enumerator next-node))
                   (union-program-node-move-next e))))))

  (cond
    ;; Initial state
    ((= -1 (%index e))
     (advance-children))
    ;; Are we still enumerating over a child?
    ((move-next (%child-enumerator e))
     t)
    ;; Otherwise, advance
    (t
     (advance-children)))))

;;;
;;; A program node representing a union of programs
;;;
(defclass union-program-node (program-node)
  ((programs :reader programs
             :initarg :programs
             :type list
             :documentation "Set of program nodes being unioned"))
  (:documentation "A program node that unions a set of program nodes"))

(defmethod initialize-instance :after ((node union-program-node) &key)
  "Filters out empty programs from this union"
  (setf (slot-value node 'programs)
        (remove-if #'is-empty-program-node? (programs node))))

(defun is-union-program-node? (node)
  "Checks if NODE is a union program node"
  (typep node 'union-program-node))

(defmethod program-count ((node union-program-node))
  "Get the number of programs rooted at this node. Sum of child nodes for unions."
  (reduce #'+ (programs node)
          :key #'program-count))

(defmethod enumerator ((node union-program-node))
  "Gets an enumerator over the union of this node's children"
  (make-instance 'union-program-node-enumerator
                 :program-list (programs node)
                 :next-pointer (programs node)
                 :reset-fn #'union-program-node-reset
                 :move-next-fn #'union-program-node-move-next
                 :current-fn #'union-program-node-current))
