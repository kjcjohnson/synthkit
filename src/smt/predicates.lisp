;;;;
;;;; Predicates - telling SMT things
;;;;
(in-package :com.kjcjohnson.synthkit.smt)

(defun is-application? (expression &optional symbol)
  "Checks if an SMT expression is a function application, optionally for the
given SMT symbol."
  (cl:and (typep expression 'expression)
          (cl:or (null symbol)
                 (eql (name expression) (ensure-identifier symbol)))))
