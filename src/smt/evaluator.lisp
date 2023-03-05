;;;;
;;;; SMT evaluator
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defun evaluate-expression (expression &optional (ctx *smt*))
  "Evaluates an SMT expression."
  ;; Just defer to the compiler
  (let* ((comp-expr (%compile-expression expression nil ctx))
         (fn (compile nil `(lambda () ,comp-expr))))
    (funcall fn)))
