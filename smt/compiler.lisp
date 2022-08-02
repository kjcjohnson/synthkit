;;;;
;;;; SMT expression compiler
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defun %compile-expression (expr args &optional (context *smt*))
  "Compiles an expression EXPR, maybe using arguments from ARGS"
  (etypecase expr
    ;; Literals - just grab the value
    (literal
     (value expr))
    (number
     expr)
    (bit-vector
     expr)
    (string
     expr)
    (boolean
     expr)

    ;; Constants - must be in the args list (for in-scope symbols)
    (constant
     (if (find (name expr) args)
         (name expr)
         (error "Argument not in scope: ~a" (name expr))))

    ;; Quantifiers - cannot be compiled
    (quantifier
     (error "Attempt to compile expression with quantifier: ~a" expr))

    ;; Function call
    (expression
     (let ((fn-defn (get-compiled-function (name expr))))
       (when (null fn-defn)
         (error "No definition for function: ~a" (name expr)))
       `(funcall ,fn-defn
                 ,@(map 'list
                        #'(lambda (e)
                            (%compile-expression e
                                                 args
                                                 context))
                        (children expr)))))))

(defun %compile-definition (defn &optional (context *smt*))
  (unless (typep defn 'lambda-binder)
    (error "Attempt to compile not a lambda binder as a definition: ~a" defn))
  `(lambda ,(arguments defn) ,(%compile-expression (body defn)
                                                   (arguments defn)
                                                   context)))
  
(defun compile-definition (defn &optional (context *smt*))
  "Compiles a function expression."
  (compile nil (%compile-definition defn context)))
