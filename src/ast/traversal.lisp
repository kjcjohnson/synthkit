;;;;
;;;; Program traversal
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

(defun traverse-program (program function type)
  "Traverses the atoms of PROGRAM, calling FUNCTION on each one (at time by TYPE)"
  (declare (type program-atom program)
           (type (function (program-atom)) function)
           (type (member :pre-order :post-order) type))
  (flet ((call-node ()
           (funcall function program))
         (iterate-children ()
           (when (typep program 'program-node)
             (loop for child in (children program)
                   do (traverse-program child function type)))))
    (case type
      (:pre-order
       (call-node)
       (iterate-children))
      (:post-order
       (iterate-children)
       (call-node)))))

(*:define-do-macro do-traverse-program ((var program type &optional return) &body body)
  `(traverse-program ,program #'(lambda (,var) ,@body) ,type))
