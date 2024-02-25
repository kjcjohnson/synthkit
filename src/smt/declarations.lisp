;;;;
;;;; declarations.lisp - handling for SMT declarations
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

;;;
;;; Function declarations
;;;
(defclass function-declaration (smt-node)
  ((name :reader name
         :initarg :name
         :initform (error "Name is required."))
   (arity :reader arity
          :initarg :arity
          :initform (error "Arity is required."))
   (return-sort :reader return-sort
                :initarg :return-sort
                :initform (error "Return sort is required."))
   (argument-sorts :reader argument-sorts
                   :initarg :argument-sorts
                   :initform (error "Argument sorts are required."))
   (arguments :reader arguments
              :initarg :arguments
              :initform nil)
   (definition :reader definition
               :initarg :definition
               :initform nil)))

(defun %write-function-signature (fn writer)
  "Writes a function signature for a definition or declaration"
  `(,(funcall writer (name fn))
    (,@(if (null (arguments fn))
           (map 'list #'(lambda (arg) (funcall writer (name arg)))
                (argument-sorts fn))
           (map 'list #'(lambda (arg sort)
                          (list (funcall writer (name arg))
                                (funcall writer (name sort))))
                (arguments fn) (argument-sorts fn))))
    ,(funcall writer (name (return-sort fn)))))

(defun %write-function-term (fn &key pprint)
  "Writes a function term for a definition"
  (to-smt (definition fn) :pprint pprint))

(defun %write-function-definition (fn writer &key pprint)
  "Writes a function definition"
  `(,(funcall writer "define-fun")
    ,@(%write-function-signature fn writer)
    ,(%write-function-term fn :pprint pprint)))

(defun %write-function-declaration (fn writer &key pprint)
  "Writes a function declaration"
  (declare (ignore pprint))
  `(,(funcall writer "declare-fun")
    ,@(%write-function-signature fn writer)))

(defmethod to-smt ((fn function-declaration) &key pprint)
  (flet ((i-or-p (thing)
           (if pprint
               (etypecase thing
                 (string thing)
                 (symbol (identifier-string thing)))
               (intern-identifier thing))))
    (if (cl:not (null (definition fn)))
        (%write-function-definition fn #'i-or-p :pprint pprint)
        (%write-function-declaration fn #'i-or-p :pprint pprint))))

(defun function-declaration (name arg-sorts ret-sort &optional arguments definition)
  (make-instance 'function-declaration
                 :name name
                 :arity (length arg-sorts)
                 :return-sort ret-sort
                 :argument-sorts arg-sorts
                 :arguments arguments
                 :definition definition))

;;;
;;; Declaration groupers
;;;
(defclass recursive-declaration-grouper (smt-node)
  ((group :reader group
          :initarg :group
          :initform nil)
   (group-type :reader group-type
               :initarg :group-type))
  (:documentation "A group of declarations that are mutually recursive"))

(defun make-rec-group (declarations)
  "Makes a recursive group of declarations"
  (declare (type (or atom list) declarations))
  (setf declarations (*:ensure-list declarations))
  (when (null declarations) (error "Attempt to make an empty declaration grouper"))
  (flet ((compute-declaration-type (decl)
           "We want to distinguish between declarations and definitions"
           (let ((type (type-of decl)))
             (if (eql type 'function-declaration)
                 (if (null (definition decl))
                     :function-declaration
                     :function-definition)
                 type))))
    (let ((group-type (compute-declaration-type (first declarations))))
      (loop for decl in (rest declarations)
            for type = (compute-declaration-type decl)
            unless (eql group-type type)
              do (error "Non-homogeneous declaration grouper: ~a and ~a"
                        group-type type))
      (make-instance 'recursive-declaration-grouper
                     :group declarations :group-type group-type))))

(defmethod copy-node ((node recursive-declaration-grouper) &key)
  "Copies a recursive declaration grouper"
  (make-instance 'recursive-declaration-grouper
                 :group (map 'list #'copy-node (group node))
                 :group-type (group-type node)))

(defmethod to-smt ((grouper recursive-declaration-grouper) &key pprint)
  "Prints a recursive grouper"
  (flet ((i-or-p (thing)
           (if pprint
               (etypecase thing
                 (string thing)
                 (symbol (identifier-string thing)))
               (intern-identifier thing))))
    (case (group-type grouper)
      (:function-definition
       `(,(i-or-p "define-funs-rec")
         (,@(map 'list (*:rcurry #'%write-function-signature #'i-or-p)
                 (group grouper)))
         (,@(map 'list (*:rcurry #'%write-function-term :pprint pprint)
                 (group grouper)))))

      (otherwise (error "Unsupported rec group type: ~a" (group-type grouper))))))
