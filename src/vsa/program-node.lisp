(in-package #:com.kjcjohnson.synthkit.vsa)

;;;
;;; Program nodes
;;;
(defclass program-node () ()
  (:documentation "An abstract program node in a VSA."))

(defun is-program-node? (obj)
  "Checks if OBJ is a program node"
  (typep obj 'program-node))

(defgeneric program-count (node)
  (:documentation "Get the number of programs rooted at this program node"))

(defgeneric enumerator (node)
  (:documentation "Get an enumerator over programs rooted at this node"))
