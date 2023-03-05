;;;;
;;;; SMT expression compiler
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defvar *%currently-compiling* nil
  "Holds a list of function definitions currently being compiled. If a function
definition appears on this list, and we try to reference it (i.e., a recursive call),
we use a form that defers fetches the definition to runtime instead of compiling in
the function object itself.")

(defvar *smt-max-stack* 200 "Max stack depth before an error is signaled")
(defvar *%smt-stack-depth* 0 "Current depth of the SMT execution stack")

(define-condition smt-max-stack-exceeded (error) ())

(defmacro with-stack-protection (&body body)
  "Inserts stack protection code"
  (if *smt-max-stack*
      `(unwind-protect
           (progn
             (incf *%smt-stack-depth*)
             (when (> *%smt-stack-depth* ,*smt-max-stack*)
               (error 'smt-max-stack-exceeded))
             ,@body)
        (decf *%smt-stack-depth*))
      `(progn ,@body)))

(defmacro with-compiling ((fn-defn) &body body)
  "Marks that the function definition FN-DEFN is being compiled"
  `(let ((*%currently-compiling* (cons ,fn-defn *%currently-compiling*)))
     ,@body))

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
     (if (eql (name expr) (ensure-identifier "ite" context))
         `(if ,@(map 'list
                     (a:rcurry #'%compile-expression args context)
                     (children expr)))
         (let ((defn (get-function-definition (name expr) context)))
           (when (null defn)
             (error "No definition for function: ~a" (name expr)))
           `(funcall ,(if (find defn *%currently-compiling*) ; handle recursive calls
                          (compiled-definition-form defn context)
                          (compiled-definition defn context))
                     ,@(map 'list
                            #'(lambda (e)
                                (%compile-expression e
                                                     args
                                                     context))
                            (children expr))))))))

(defvar *smt-function-trace* nil "If T, generates tracing code for SMT functions")
(defvar *%smt-function-trace-depth* 0)

(defun %compile-definition (defn &optional (context *smt*))
  (unless (typep defn 'lambda-binder)
    (error "Attempt to compile not a lambda binder as a definition: ~a" defn))
  (if *smt-function-trace*
      `(lambda ,(arguments defn)
         (format *trace-output* "~&;SMT -> [~a] ~s~%" "???" (list ,@(arguments defn)))
         (let ((retval
                 (with-stack-protection
                   ,(%compile-expression (body defn)
                                         (arguments defn)
                                         context))))
           (format *trace-output* "~&;SMT <- [~a] ~s~%" "???" retval)
           retval))
      `(lambda ,(arguments defn)
         (with-stack-protection
           ,(%compile-expression (body defn)
                                 (arguments defn)
                                 context)))))

(defgeneric compile-definition (definition &optional context)
  (:documentation "Compiles a definition object"))

(defmethod compile-definition ((defn lambda-binder) &optional (context *smt*))
  "Compiles a function expression."
  (compile nil (%compile-definition defn context)))

(defmethod compile-definition ((defn function-definition) &optional (context *smt*))
  "Compiles a function definition object"
  (with-compiling (defn)
    (compile-definition (definition defn) context)))
