;;;;
;;;; Operations on CHCs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.chc)

(defun fixup-forward-declared-head (relation new-head)
  "Fixes up a forward-declared head in RELATION with NEW-HEAD."
  (declare (type relation relation)
           (type head new-head))
  (assert (typep (head relation) 'forward-declared-head))
  (assert (eql (name (head relation)) (name new-head)))
  (setf (slot-value relation 'head) new-head))

(defun make-head-from-relation (relation symbol-table)
  "Creates a CHC head from a relation object"
  (check-type relation relation)
  (check-type symbol-table symbol-table)
  (let ((roles (make-array (length (signature relation)) :initial-element :unknown)))
    (labels ((add-role (role se)
               (setf (svref roles (symbol-index se)) role))
             (add-roles (role entries)
               (map nil (a:curry #'add-role role) entries)))
      (add-roles :input (input-symbols symbol-table))
      (add-roles :output (output-symbols symbol-table))
      (add-role :term (term-symbol symbol-table)))
    (make-instance 'head
                   :name (name relation)
                   :signature (signature relation)
                   :roles roles
                   :formals (actuals relation))))
