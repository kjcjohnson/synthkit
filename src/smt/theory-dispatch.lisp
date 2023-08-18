;;;;
;;;; Theory dispatch - looks up function for symbol
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *builtin-smt-functions* (make-hash-table :test 'equal)))

;;;
;;; Types for names and indexes
;;;
(deftype smt-index-type (&key (concrete t))
  `(or string integer ,@(unless concrete '(symbol))))

(defun %smt-abstract-name-type-p (rest)
  "Checks that REST satisfies the constraints to be an index"
  (every #'(lambda (x) (typep x '(smt-index-type :concrete nil))) rest))

(defun %smt-concrete-name-type-p (rest)
  "Checks that REST satisfies the contraints to be a concrete index"
  (every #'(lambda (x) (typep x '(smt-index-type :concrete t))) rest))

(deftype smt-simple-name-type ()
  'string)

(deftype smt-indexed-name-type (&key (concrete t))
  `(cons string (satisfies ,(if concrete
                                '%smt-concrete-name-type-p
                                '%smt-abstract-name-type-p))))

(deftype smt-name-type (&key (concrete t))
  `(or smt-simple-name-type (smt-indexed-name-type :concrete ,concrete)))

;;;
;;; Classes for theory declarations
;;;
(defclass theory-entry ()
  ((smt-name :reader smt-name
             :initarg :smt-name
             :type smt-name-type
             :documentation "The SMT name of this entry")
   (fn-name :reader fn-name
            :initarg :fn-name
            :type symbol
            :documentation "The name of the function underlying this entry")
   (fn-impl :reader fn-impl
            :initarg :fn-impl
            :type function
            :documentation "The function implementation for this entry")
   (fn-theory :reader fn-theory
              :initarg :fn-theory
              :type keyword
              :documentation "The theory that this entry belongs to"))
  (:documentation "A theory function entry"))

(defclass theory-family-entry (theory-entry)
  ((param-indexes :reader param-indexes
                  :initarg :param-indexes
                  :type list
                  :documentation "The index of symbols in the index that are args")
   (lambda-list :reader lambda-list
                :initarg :lambda-list
                :type list
                :documentation "The lambda list of the family implementation"))
  (:documentation "A family of theory functions, i.e., indexed identifiers"))

(defclass theory-fn-entry (theory-entry)
  ()
  (:documentation "A concrete theory function entry"))

(defun indexed-base-name (smt-name &key concrete)
  (cond
    ((atom smt-name)
     (format nil "~a" smt-name))
    ((= 1 (length smt-name))
     (format nil "~a" (first smt-name)))
    (concrete
     (format nil "~a" smt-name))
    (t
     (format nil "~a;~a" (first smt-name) (length (rest smt-name))))))

(defun %parse-indexed-name (smt-name)
  "Parses data out of SMT-NAME"
  (setf smt-name (a:ensure-list smt-name))
  (let ((concrete (not (some #'symbolp (rest smt-name))))
        (param-ixs nil)
        (param-syms nil))
    (loop for ix from 0
          for x in (rest smt-name)
          when (symbolp x)
            collect ix into indexes
            and collect x into symbols
          finally (setf param-ixs indexes
                        param-syms symbols))
    (let ((base-name (indexed-base-name smt-name :concrete concrete)))
      (values base-name concrete param-ixs param-syms))))

(defmacro defsmtfun (smt-name theory lambda-list &body body)
  "Defines a built-in SMT theory function named SMT-NAME. This function will be
associated with a theory THEORY (denoted by a keyword) with given LAMBDA-LIST), and an
implementation given by BODY."
  (check-type smt-name (smt-name-type :concrete nil))
  (check-type theory keyword)
  (setf smt-name (a:ensure-list smt-name))
  (multiple-value-bind (base-name concrete param-ixs param-syms)
      (%parse-indexed-name smt-name)
    (let ((fn-name (intern (format nil "SMT[~a]~a~@[[~{~a~^,~}]~]"
                                   theory smt-name param-ixs))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defun ,fn-name ,(append param-syms lambda-list)
           ,@body)
         (setf (gethash ',base-name *builtin-smt-functions*)
               ,(if concrete
                    `(make-instance 'theory-fn-entry
                                    :smt-name ',smt-name
                                    :fn-name ',fn-name
                                    :fn-impl #',fn-name
                                    :fn-theory ,theory)
                    `(make-instance 'theory-family-entry
                                    :smt-name ',smt-name
                                    :fn-name ',fn-name
                                    :fn-impl #',fn-name
                                    :fn-theory ,theory
                                    :param-indexes ',param-ixs
                                    :lambda-list ',lambda-list)))))))

(defun %compile-concrete-entry (family name)
  "Compiles a concrete SMT function definition from FAMILY and NAME"
  ;; TODO: parse lambda list and apply properly
  (let ((ix-args (loop for ix in (param-indexes family)
                       collect (elt name (1+ ix))))) ; Skip base name
    (eval `(defsmtfun ,name ,(fn-theory family) ,(lambda-list family)
             (,(fn-name family) ,@ix-args ,@(lambda-list family))))))

(defun %lookup-theory-entry (name)
  "Looks up a theory entry for NAME. If an abstract family, compiles a concrete entry"
  (declare (type smt-name-type name))
  (setf name (a:ensure-list name))
  (if (= 1 (length name))
      (gethash (indexed-base-name name) *builtin-smt-functions*)
      (let* ((concrete (indexed-base-name name :concrete t))
             (looked-up (gethash concrete *builtin-smt-functions*)))
        (if looked-up
            looked-up
            (let* ((abstract (indexed-base-name name :concrete nil))
                   (looked-up (gethash abstract *builtin-smt-functions*)))
              (if looked-up
                  (%compile-concrete-entry looked-up name)
                  nil))))))

(defun lookup-theory-function (name)
  "Looks up an SMT theory function"
  (declare (type smt-name-type name))
  (let ((looked-up (%lookup-theory-entry name)))
    (unless (null looked-up)
      (return-from lookup-theory-function (fn-impl looked-up))))

  ;; Not a theory function
  nil)

(defun map-built-in-definitions (operation)
  "Iterates over built-in function definitions. OPERATION accepts two args"
  (maphash #'(lambda (key value)
               (funcall operation key (fn-impl value)))
           *builtin-smt-functions*))

(defun lookup-theory-function-symbol (name)
  "Looks up a function name for a theory function"
  (declare (type smt-name-type name))
  (let ((looked-up (%lookup-theory-entry name)))
    (if (null looked-up)
        nil
        (fn-name looked-up))))

(defun do-call-smt (fnname &rest args)
  "Calls an SMT function with arguments ARGS. Looks up FNNAME at runtime - prefer using
the macros CALL-SMT and APPLY-SMT if possible to do the lookup at compile time."
  (apply (lookup-theory-function fnname) args))

(defun %maybe-lookup-inline-theory-function (fnname env)
  "Attempts to look up the SMT theory function with name FNNAME. If FNNAME is a
constant string which names an SMT theory function, that function symbol will be
returned, or a concrete indexed identifier, otherwise NIL."
  (when (typep fnname '(smt-name-type :concrete t) env)
    (a:when-let (fn-symb (lookup-theory-function-symbol fnname))
      (return-from %maybe-lookup-inline-theory-function fn-symb)))

  (warn "Cannot inline call to SMT theory function: ~a" fnname)
  nil)

(defun %generate-smt-funcall (fnname args env)
  "Generates a form to call an SMT function with name FNNAME and arguments ARGS."
  (a:if-let (fn-symb (%maybe-lookup-inline-theory-function fnname env))
    `(,fn-symb ,@args)
    `(funcall (lookup-theory-function ',fnname) ,@args)))

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
