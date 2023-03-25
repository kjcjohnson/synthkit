(in-package #:com.kjcjohnson.synthkit.vsa)

;;;
;;; Empty program node
;;;
(defclass empty-program-node (program-node)
  ()
  (:documentation "A program node holding no children"))

(let ((epn (make-instance 'empty-program-node)))
  (defun make-empty-program-node ()
    "Makes an empty program node. May or may not be shared."
    epn))

(defun is-empty-program-node? (node)
  "Checks if NODE is an empty program node"
  (typep node 'empty-program-node))

(defmethod program-count ((node empty-program-node))
  "Get the number of programs rooted at this node. Since this node is empty, always 0."
  (declare (ignore node))
  0)

(defmethod enumerator ((node empty-program-node))
  "Get an enumerator that has no elements."
  (make-instance 'program-node-enumerator
                 :reset-fn (constantly nil)
                 :move-next-fn (constantly nil)
                 :current-fn (constantly nil)))
