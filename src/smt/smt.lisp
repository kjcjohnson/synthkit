;;;
;;; smt - SMT-LIB2 functionality
;;;
(in-package #:com.kjcjohnson.synthkit.smt)

;;
;; Sorts: Int, Bool, String
;; Declaring variables of these types
;;
(defclass sort ()
  ((name :reader name
         :initarg :name
         :initform (error "Sort name is required."))
   (parameters :reader sort-parameters
               :initarg :parameters
               :type list
               :initform nil
               :documentation "Any sort parameters associated with this sort")))

(defmethod print-object ((sort sort) stream)
  (print-unreadable-object (sort stream :type t)
    (prin1 (name sort) stream)))

(defmethod arity ((sort sort))
  "Gets the arity of a sort - the number of parameters it has"
  (length (sort-parameters sort)))

(defparameter *int-sort* (make-instance 'sort :name "Int"))
(defparameter *bool-sort* (make-instance 'sort :name "Bool"))
(defparameter *string-sort* (make-instance 'sort :name "String"))
(defparameter *reglan-sort* (make-instance 'sort :name "RegLan"))

;;
;; Solvers and solving
;;
(defclass solver* ()
  ((program :reader program :initarg :program)
   (arguments :reader arguments :initarg :arguments)))

(defclass solver-configuration (solver*)
  ()
  (:documentation "Transitional class renaming"))

(defparameter *cvc4* (make-instance 'solver-configuration
                                    :program #P"d:/bin/cvc4-1.8-win64-opt.exe"
                                    :arguments (list "--lang" "smt2"
                                                     "--produce-models"
                                                     "--incremental"
                                                     "--tlimit-per" "10000"
                                                     )))

(defparameter *cvc5* (make-instance 'solver-configuration
                                    :program "cvc5"
                                    :arguments (list "--lang" "smt2"
                                                     "--produce-models"
                                                     "--incremental"
                                                     "--tlimit-per" "10000"
                                                     )))

(defun declare-constants (solver formula)
  "Declares all constants in the given formula."
  (assert-smt-solver-enabled)
  (let ((constants (find-constants formula)))
    (apply #'declare-constant solver constants)))

(defun dump-commands (solver commands)
  (assert-smt-solver-enabled)
    (cl-smt-lib:write-to-smt solver commands))

(defgeneric copy-node (node &key &allow-other-keys)
  (:documentation "Makes a shallow copy of an SMT node"))

(defclass smt-node ()
  ())

(defclass term (smt-node)
  ((name :reader name
         :initarg :name
         :initform (error "Name is required.")
         :documentation "This term's name or representative value")
   (sort :reader sort
         :initarg :sort
         :initform (error "Sort is required.")))
  (:documentation "An SMT term, which is of a particular SORT"))

(defclass expression (term)
  ((name :reader name :initarg :name :initform (error "Name is required."))
   (sort :reader sort :initarg :sort :initform (error "Sort is required."))))

(defclass application (expression)
  ((arity :reader arity
          :initarg :arity
          :initform (error "Arity is required."))
   (children :reader children
             :initarg :children
             :initform (error "Children are required."))
   (child-sorts :reader child-sorts
                :initarg :child-sorts
                :initform (error "Child sorts are required.")))
  (:documentation "An application of a function named NAME to arguments CHILDREN"))

(defmethod copy-node ((node application) &key children)
  "Copies an SMT expression"
  (make-instance 'application
                 :name (name node)
                 :sort (sort node)
                 :arity (arity node)
                 :children (if children children (copy-list (children node)))
                 :child-sorts (copy-list (child-sorts node))))

(defclass constant (application)
  ()
  (:default-initargs :arity 0 :children nil :child-sorts nil))

(defmethod copy-node ((node constant) &key)
  "Copies a constant node"
  (make-instance 'constant
                 :name (name node)
                 :sort (sort node)))


(defun set-model (solver model)
  "Defines constants for each variable in the model"
  (assert-smt-solver-enabled)
  (cl-smt-lib:write-to-smt solver
                           (map 'list #'(lambda (m)
                                          (destructuring-bind (var . value) m
                                            (check-type var constant)
                                            (list (intern "define-const")
                                                  (intern (name var))
                                                  (intern (name (sort var)))
                                                  value)))
                                model)))


(defclass literal (term)
  ((value :reader value :initarg :value :initform (error "Literal value is required."))))

(defmethod arity ((node literal)) 0)

(defmethod copy-node ((node literal) &key)
  "Copies a constant node"
  (make-instance 'literal
                 :name (name node)
                 :sort (sort node)
                 :value (value node)))

(defclass quantifier (expression)
  ((arity :initform 1 :reader arity)
   (arguments :reader arguments
              :initarg :arguments
              :initform (error "Arguments are required."))
   (argument-sorts :reader argument-sorts
                   :initarg :argument-sorts
                   :initform (error "Argument sorts are required."))
   (sort :initform *bool-sort*)
   (children :reader children
             :initarg :children
             :initform (error "Children (single child) is required."))
   (child-sorts :reader child-sorts
                :initarg :child-sorts
                :initform (error "Child sorts are required."))))


(defmethod copy-node ((node quantifier) &key children)
  "Copies a quantifier node"
  (make-instance 'quantifier
                 :name (name node)
                 :sort (sort node)
                 :arguments (copy-list (arguments node))
                 :argument-sorts (copy-list (argument-sorts node))
                 :children (if children children (copy-list (children node)))
                 :child-sorts (copy-list (child-sorts node))))

(defclass lambda-binder (smt-node)
  ((arguments :reader arguments
              :initarg :arguments
              :initform (error "Arguments are required."))
   (body :reader body
         :initarg :body
         :initform (error "Body is required."))))

(defun make-lambda-binder (arguments body)
  "Creates a lambda binder term, with ARGUMENTS and BODY."
  (make-instance 'lambda-binder :arguments arguments :body body))

(defmethod print-object ((constant constant) stream)
  (print-unreadable-object (constant stream :type t)
    (format stream "~A : ~A" (name constant) (name (sort constant)))))

(defgeneric to-smt (expr &key pprint))
(defmethod to-smt ((constant constant) &key pprint)
  (if pprint
      (identifier-string (name constant))
      (intern (identifier-smt (name constant)))))
(defmethod to-smt ((integer integer) &key pprint)
  (declare (ignore pprint))
  (if (< integer 0)
      `(- ,(- integer))
      integer))

(defun intern-identifier (identifier)
  "Interns an identifier. Gross, but we need it for now"
  (let ((ismt (identifier-smt identifier)))
    (if (listp ismt)
        (list* (intern "_")
               (map 'list #'(lambda (x)
                              (cond ((stringp x) (intern x))
                                    ((integerp x) x)
                                    (t (error "Cannot intern index: ~a" x))))
                    ismt))
        (intern ismt))))

(defmethod to-smt ((expression expression) &key pprint)
  (if (zerop (length (children expression)))
      (intern-identifier (name expression))
      `(,(intern-identifier (name expression))
        ,@(map 'list (a:rcurry #'to-smt :pprint pprint) (children expression)))))

(defmethod to-smt ((lit literal) &key pprint)
  ;; Special case for Boolean literals
  (if pprint
      (if (eql (sort lit) *bool-sort*)
          (let ((val (string-downcase (symbol-name (value lit)))))
            (cond
              ((string-equal val "true") "true")
              ((string-equal val "false")"false")
              (t (error "Illegal Boolean value: ~a" val))))
          (error "Unhandled pprint literal: ~a" lit))
      (value lit)))

(defclass smt-bv-wrapper ()
  ((bv :reader %bv
       :initarg :bv
       :type bit-vector
       :documentation "Underlying bit vector"))
  (:documentation "Wrapper class to facilitate printing bit vectors"))

(defmethod print-object ((bvw smt-bv-wrapper) stream)
  (format stream "~a" (to-smt (%bv bvw) :pprint t)))

(defmethod to-smt ((bv bit-vector) &key pprint)
  (if pprint
      (let ((str (format nil "~a" bv)))
        (setf (elt str 1) #\b) ; Just need to swap * for b
        str)
      (make-instance 'smt-bv-wrapper :bv bv)))

(defmethod sort ((bv bit-vector))
  (ensure-sort (ensure-identifier (list "BitVec" (length bv)))))

(defmethod to-smt (l &key pprint)
  (declare (ignore pprint))
  l) ; dump anything directly if no applicable method

(defun maybe-quote (name)
  (if (symbolp name)
      `(quote ,name)
      name))

(defmethod name ((symbol symbol)) symbol)

(defmethod sort ((int integer)) *int-sort*)
(defmethod arity ((int integer)) 0)
(defmethod name ((int integer)) int)

(defmethod print-object ((expression expression) stream)
  (print-unreadable-object (expression stream :type t)
    (format stream "(~A~{~^ ~A~}) : ~A"
            (name expression)
            (children expression)
            (name (sort expression)))))

(defun find-constants (expr)
  "Finds all (unique) constants in an expression."
  (unless (subtypep (type-of expr) 'smt-node)
    (return-from find-constants nil))
  (let ((result nil))
    (if (zerop (arity expr))
        (when (typep expr 'constant) (pushnew expr result :key #'name :test #'equal))
        (dolist (child (children expr))
          (dolist (c (find-constants child))
            (pushnew c result :key #'name :test #'equal))))
    result))

(defun variable (name sort)
  "Returns a variable of the given name and sort."
  (make-instance 'constant :name name :sort sort))

(defun int-variable (name)
  "Returns an integer variable of the given name."
  (variable name *int-sort*))

(defun bool-variable (name)
  "Returns a boolean variable of the given name."
  (variable name *bool-sort*))

(defun string-variable (name)
  "Returns a string variable of the given name."
  (variable name *string-sort*))

(defun quantifier-expression (type arguments sorts expression)
  (assert (= (length arguments) (length sorts)) nil "Arguments and sorts must have the same length.")
  (make-instance 'quantifier
                 :name type
                 :arguments arguments
                 :argument-sorts sorts
                 :children (list expression)
                 :child-sorts (list (sort expression))))

(defmethod to-smt ((expr quantifier) &key pprint)
  (assert (cl:or (string= (name expr) "forall")
              (string= (name expr) "exists")) nil
          "Quantifiers must be 'forall' or 'exists', but got: ~S" (name expr))
  `(,(intern (name expr)) (,@(map 'list
                                  #'(lambda (a s)
                                      (list
                                       (intern (identifier-smt a))
                                       (intern-identifier (name s))))
                                  (arguments expr)
                                  (argument-sorts expr)))
    ,(to-smt (first (children expr)) :pprint pprint)))
