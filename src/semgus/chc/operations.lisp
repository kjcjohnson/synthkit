;;;;
;;;; Operations on CHCs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.chc)

(defun production-for-chc (chc grammar)
  "Gets the production associated with the CHC in the grammar"
  (find (name (constructor chc))
        (g:productions grammar)
        :test (lambda (name prod)
                (eql name (g:name (g:operator prod))))))

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

(defun body-to-smt (chc)
  "Converts a CHC body to an SMT expression"
  (let ((expr (apply #'smt:$and
                  (constraint chc)
                  (map 'list
                       #'(lambda (b)
                           (make-instance 'smt::expression
                                          :name (name b)
                                          :arity (length (actuals b))
                                          :children
                                          (map 'list
                                               #'(lambda (a s)
                                                   (smt:variable a s))
                                               (actuals b)
                                               (signature b))
                                          :child-sorts (signature b)
                                          :sort smt:*bool-sort*))
                       (body chc)))))

    (unless (zerop (length (auxiliary-symbols chc)))
      (setf expr
            (smt::quantifier-expression
             "exists"
             (map 'list #'symbol-name (auxiliary-symbols chc))
             (map 'list #'symbol-sort (auxiliary-symbols chc))
             expr)))
    expr))
