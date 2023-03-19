(in-package #:com.kjcjohnson.synthkit.vsa)

(defclass leaf-program-node-enumerator (program-node-enumerator)
  ((started :accessor %started :initarg :started)
   (program :accessor %program :initarg :program))
  (:default-initargs :started nil)
  (:documentation "Singleton enumerator for leaf program nodes"))

(defun leaf-program-node-reset (e)
  (setf (%started e) nil))

(defun leaf-program-node-move-next (e)
  (not (shiftf (%started e) t)))

(defun leaf-program-node-current (e)
  (%program e))

;;;
;;; A program node representing a single concrete program
;;;
(defclass leaf-program-node (program-node)
  ((program :reader program
            :initarg :program
            :type ast:program-node
            :documentation "The concrete program associated with this node"))
  (:documentation "A program node holding a single, concrete program"))

(defun is-leaf-program-node? (node)
  "Checks if NODE is a leaf program node."
  (typep node 'leaf-program-node))

(defmethod program-count ((node leaf-program-node))
  "Get the number of programs rooted at NODE. As this is a leaf node, always 1."
  (declare (ignore node))
  1)

(defmethod enumerator ((node leaf-program-node))
  "Get an enumerator for a singleton program node"
  (make-instance 'leaf-program-node-enumerator
                 :reset-fn #'leaf-program-node-reset
                 :move-next-fn #'leaf-program-node-move-next
                 :current-fn #'leaf-program-node-current
                 :program (program node)))

