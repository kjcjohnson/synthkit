;;;;
;;;; Theory dispatch - looks up function for symbol
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *builtin-smt-functions* nil))

(defmacro defsmtfun (smt-name theory lambda-list &body body)
  "Defines a built-in SMT theory function named SMT-NAME. This function will be
associated with a theory THEORY (denoted by a keyword) with given LAMBDA-LIST), and an
implementation given by BODY."
  (check-type smt-name string)
  (check-type theory keyword)
  (let ((fn-name (intern (format nil "SMT[~a]~a" theory smt-name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,fn-name ,lambda-list
         ,@body)
       (a:if-let (cell (assoc ,smt-name *builtin-smt-functions* :test #'string=))
         (setf (cdr cell) (cons ',fn-name #',fn-name))
         (setf *builtin-smt-functions*
               (acons ,smt-name
                      (cons ',fn-name #',fn-name)
                      *builtin-smt-functions*))))))

(defun lookup-theory-function (name)
  "Looks up an SMT theory function"
  (flet ((ensure-list (thing)
           "Ensures that THING is a list. Makes a single element list if not."
           (if (consp thing)
               thing
               (list thing))))

    (setf name (ensure-list name))
    (let ((looked-up (assoc name *builtin-smt-functions*
                            :key #'ensure-list
                            :test #'equal)))
      (unless (null looked-up)
        (return-from lookup-theory-function (cddr looked-up))))

    ;; Not a theory function
    nil))

(defun map-built-in-definitions (operation)
  "Iterates over built-in function definitions. OPERATION accepts two args"
  (dolist (defn-form *builtin-smt-functions*)
    (funcall operation (car defn-form) (cdr (cdr defn-form)))))

(defun lookup-theory-function-symbol (name)
  "Looks up a function name for a theory function"
  (let ((looked-up (cdr (assoc name *builtin-smt-functions*
                               :test #'equal))))
    (if (null looked-up)
        nil
        (car looked-up))))

(defun do-call-smt (fnname &rest args)
  "Calls an SMT function with arguments ARGS. Looks up FNNAME at runtime - prefer using
the macros CALL-SMT and APPLY-SMT if possible to do the lookup at compile time."
  (apply (lookup-theory-function fnname) args))

(defun %maybe-lookup-inline-theory-function (fnname env)
  "Attempts to look up the SMT theory function with name FNNAME. If FNNAME is a
constant string which names an SMT theory function, that function symbol will be
returned, otherwise NIL."
  (when (and (constantp fnname env)
             (stringp fnname))
    (a:when-let (fn-symb (lookup-theory-function-symbol fnname))
      (return-from %maybe-lookup-inline-theory-function fn-symb)))

  (warn "Cannot inline call to SMT theory function: ~a" fnname)
  nil)

(defun %generate-smt-funcall (fnname args env)
  "Generates a form to call an SMT function with name FNNAME and arguments ARGS."
  (a:if-let (fn-symb (%maybe-lookup-inline-theory-function fnname env))
    `(,fn-symb ,@args)
    `(funcall (lookup-theory-function ,fnname) ,@args)))

(defun %generate-smt-apply (fnname arg-list env)
  "Generates a form to call an SMT function (as if by APPLY) with name FNNAME and
arguments in list ARG-LIST in environment ENV."
  (a:if-let (fn-symb (%maybe-lookup-inline-theory-function fnname env))
    `(apply #',fn-symb ,arg-list)
    `(apply (lookup-theory-function ,fnname) ,arg-list)))

(defmacro call-smt (fnname &rest args &environment env)
  "Calls an SMT theory function named FNNAME with arguments ARGS."
  (%generate-smt-funcall fnname args env))

(defmacro apply-smt (fnname args &environment env)
  "Calls an SMT theory function named FNNAME (as if by APPLY) with arguments ARGS."
  (%generate-smt-apply fnname args env))