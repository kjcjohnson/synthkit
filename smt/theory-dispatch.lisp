;;;;
;;;; Theory dispatch - looks up function for symbol
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *builtin-smt-functions* nil))

(defmacro defsmtfun (smt-name theory lambda-list &body body)
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
  

(let ((data
        `(
          ;; Core functions
          ("true"     . ,#'core-true)
          ("false"    . ,#'core-false)
          ("not"      . ,#'core-not)
          ("or"       . ,#'core-or)
          ("and"      . ,#'core-and)
          ("=>"       . ,#'core-=>)
          ("xor"      . ,#'core-xor)
          ("="        . ,#'core-=)
          ("distinct" . ,#'core-distinct)
          ("ite"      . ,#'core-ite)
          
          ;; Integer functions
          ("+" . ,#'ints-+)
          ("-" . ,#'ints--)
          ("<" . ,#'ints-<)
          (">" . ,#'ints->)
          ("*" . ,#'ints-*)
          
          ;; Bit vector functions
          ("bvnot"  . ,#'bv-not)
          ("bvand"  . ,#'bv-and)
          ("bvor"   . ,#'bv-or)
          ("bvxor"  . ,#'bv-xor)
          ("bvlshr" . ,#'bv-lshr)
          ("bvshl"  . ,#'bv-shl)
          ("bvadd"  . ,#'bv-add)
          )))
  
  (defun lookup-theory-function (name)
    "Looks up an SMT theory function"
    (flet ((ensure-list (thing)
             "Ensures that THING is a list. Makes a single element list if not."
             (if (consp thing)
                 thing
                 (list thing))))
      
      (setf name (ensure-list name))
      
      (let ((looked-up (assoc name data
                              :key #'ensure-list
                              :test #'equal)))
        
        (unless (null looked-up)
          (return-from lookup-theory-function (cdr looked-up)))

        (let ((looked-up (assoc name *builtin-smt-functions*
                                :key #'ensure-list
                                :test #'equal)))
          (unless (null looked-up)
            (return-from lookup-theory-function (cddr looked-up))))
        
        ;; Not a theory function
        nil)))

  (defun map-built-in-definitions (operation)
    "Iterates over built-in function definitions. OPERATION accepts two args"
    (dolist (defn-form *builtin-smt-functions*)
      (funcall operation (car defn-form) (cdr (cdr defn-form))))
    (dolist (defn-form data)
      (funcall operation (car defn-form) (cdr defn-form)))))


(defun lookup-theory-function-symbol (name)
  "Looks up a function name for a theory function"
  (let ((looked-up (cdr (assoc name *builtin-smt-functions*
                               :test #'equal))))
    (if (null looked-up)
        nil
        (car looked-up))))

(defun do-call-smt (fnname &rest args)
  (apply (lookup-theory-function fnname) args))

(defmacro call-smt (fnname &rest args &environment env)
  (when (and (constantp fnname env)
             (stringp fnname))
    (a:when-let (fn-symb (lookup-theory-function-symbol fnname))
      (return-from call-smt `(,fn-symb ,@args))))

  `(funcall (lookup-theory-function ,fnname) ,@args))
