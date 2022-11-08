;;;
;;; smt - SMT-LIB2 functionality
;;;
(in-package #:com.kjcjohnson.synthkit.smt)

;;
;; Sorts: Int, Bool, String
;; Declaring variables of these types
;;
(defclass sort ()
  ((name :reader name :initarg :name :initform (error "Sort name is required."))))

(defmethod print-object ((sort sort) stream)
  (print-unreadable-object (sort stream :type t)
    (prin1 (name sort) stream)))

(defparameter *int-sort* (make-instance 'sort :name "Int"))
(defparameter *bool-sort* (make-instance 'sort :name "Bool"))
(defparameter *string-sort* (make-instance 'sort :name "String"))

;;
;; Solvers and solving
;;
(defclass solver ()
  ((program :reader program :initarg :program)
   (arguments :reader arguments :initarg :arguments)))

(defparameter *cvc4* (make-instance 'solver
                                    :program #P"d:/bin/cvc4-1.8-win64-opt.exe"
                                    :arguments (list "--lang" "smt2"
                                                     "--produce-models"
                                                     "--incremental"
                                                     "--tlimit-per" "10000"
                                                     )))
(format t "~&***** FEATURES: ~s~%" *features*)
(defun assert-smt-solver-enabled ()
  "Checks if SMT solving is enabled."
  #+synthkit-disable-smt-solver
  (error "SMT solver support is disabled."))

#+synthkit-disable-smt-solver
(defmacro with-solver ((solver solver-spec) &body body)
  (warn "~&; SMT solving disabled in synthkit~%")
  `(let ((solver nil))
     ,@body))

#-synthkit-disable-smt-solver
(defmacro with-solver ((solver solver-spec) &body body)
  (warn "~&; SMT solving NOT disabled in synthkit~%")
  (let ((form (gensym))
        (result (gensym))
        (status (gensym)))
    `(with-open-stream (,solver (apply #'cl-smt-lib:make-smt (program ,solver-spec) (arguments ,solver-spec)))
       (unwind-protect
            (progn
              (let ((,result (progn
                               ,@body)))
                (close (cl-smt-lib/process-two-way-stream:output ,solver))
                (let ((,status (uiop:wait-process (cl-smt-lib/process-two-way-stream:process ,solver))))
                  (unless (zerop ,status) (error "SMT solver failed with exit status ~S" ,status))
                  (values ,result
                          (loop :for ,form = (cl-smt-lib:read-from-smt ,solver t nil :eof)
                                :while (cl:not (equal :eof ,form))
                                :collect ,form)
                          ,status))))
         ;; Ensure the process is terminated.
         ;(format *trace-output* "; Terminating SMT process...~%")
         (uiop:terminate-process (cl-smt-lib/process-two-way-stream:process ,solver))))))

(defun make-solver (solver-spec)
  (assert-smt-solver-enabled)
  (apply #'cl-smt-lib:make-smt (program solver-spec) (arguments solver-spec)))

(defun close-solver (solver)
  (assert-smt-solver-enabled)
  (close (cl-smt-lib/process-two-way-stream:output solver))
  (let ((status (uiop:wait-process (cl-smt-lib/process-two-way-stream:process solver))))
    (unless (zerop status) (error "SMT solver failed with exit status ~S" status)))
  (prog1
      (loop :for form = (cl-smt-lib:read-from-smt solver t nil :eof)
            :while (cl:not (equal :eof form))
            :collect form)
    ;; Ensure the process is terminated.
    (uiop:terminate-process (cl-smt-lib/process-two-way-stream:process solver))))

(defun declare-constants (solver formula)
  "Declares all constants in the given formula."
  (assert-smt-solver-enabled)
  (let ((constants (find-constants formula)))
    (dolist (c constants)
      (cl-smt-lib:write-to-smt solver `((,(intern "declare-const") ,(intern (name c)) ,(intern (name (sort c)))))))))

(defun solve (solver-spec &rest assertions)
  "Solves the given SMT query."
  (assert-smt-solver-enabled)
  (with-solver (smt solver-spec)
    (let ((constants (reduce #'append (map 'list #'find-constants assertions))))
      (dolist (c constants)
        (cl-smt-lib:write-to-smt smt `((|declare-const| ,(intern (name c)) ,(intern (name (sort c))))))))
    (apply #'add smt assertions)
    (let ((sat (check-sat smt)))
      (prog1
          (if (eql :sat sat) (get-model smt) sat)
        (cl-smt-lib:write-to-smt smt `((|exit|)))))))

(defun push-scope (solver)
  (assert-smt-solver-enabled)
  (cl-smt-lib:write-to-smt solver `((,(intern "push") 1))))

(defun pop-scope (solver)
  (assert-smt-solver-enabled)
  (cl-smt-lib:write-to-smt solver `((,(intern "pop") 1))))

(defmacro with-scope ((solver) &body body)
  "Executes BODY in a new SMT scope."
  `(progn
     (push-scope ,solver)
     (unwind-protect
          (progn ,@body)
       (pop-scope ,solver))))

(defun add (solver &rest assertions)
  (assert-smt-solver-enabled)
  (let ((as (intern "assert")))
    (dolist (a assertions)
      (cl-smt-lib:write-to-smt solver `((,as ,(to-smt a)))))))

(defun dump-commands (solver commands)
  (assert-smt-solver-enabled)
    (cl-smt-lib:write-to-smt solver commands))

(defun check-sat (solver)
  (assert-smt-solver-enabled)
  (cl-smt-lib:write-to-smt solver `((,(intern "check-sat"))))
  (let ((result (cl-smt-lib:read-from-smt solver t)))
    (cond ((eql (intern "sat") result) :sat)
          ((eql (intern "unsat") result) :unsat)
          (t :unknown))))

(defun get-model (solver)
  (assert-smt-solver-enabled)
  (cl-smt-lib:write-to-smt solver `((,(intern "get-model"))))
  (let ((output (cl-smt-lib:read-from-smt solver t)))
    (assert (string= "model" (symbol-name (first output))))
    (map 'list #'(lambda (dfn)
                   (destructuring-bind (df-kw name args type value) dfn
                     (declare (ignore args))
                     (assert (string= "define-fun" (symbol-name df-kw)))
                     (cons (variable name (make-instance 'sort :name (symbol-name type))) value)))
         (rest output))))

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

(defclass smt-node ()
  ())

(defclass expression (smt-node)
  ((name :reader name :initarg :name :initform (error "Name is required."))
   (sort :reader sort :initarg :sort :initform (error "Sort is required."))
   (arity :reader arity :initarg :arity :initform (error "Arity is required."))
   (children :reader children :initarg :children :initform (error "Children are required."))
   (child-sorts :reader child-sorts :initarg :child-sorts :initform (error "Child sorts are required."))))

(defclass constant (expression)
  ((arity :initarg nil :initform 0)
   (children :initarg nil :initform nil)
   (child-sorts :initarg nil :initform nil)))

(defclass literal (expression)
  ((arity :initarg nil :initform 0)
   (children :initarg nil :initform nil)
   (child-sorts :initarg nil :initform nil)
   (value :reader value :initarg :value :initform (error "Literal value is required."))))

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

(defclass quantifier (expression)
  ((arity :initform 1)
   (arguments :reader arguments
              :initarg :arguments
              :initform (error "Arguments are required."))
   (argument-sorts :reader argument-sorts
                   :initarg :argument-sorts
                   :initform (error "Argument sorts are required."))
   (sort :initform *bool-sort*)
   (children :initarg children :initform (error "Children (single child) is required."))))

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

(defgeneric to-smt (expr))
(defmethod to-smt ((constant constant))
  (intern (name constant)))
(defmethod to-smt ((integer integer))
  integer)
(defmethod to-smt ((expression expression))
  `(,(intern (name expression)) ,@(map 'list #'to-smt (children expression))))
(defmethod to-smt ((fn function-declaration))
  (if (cl:not (null (definition fn)))
      `(,(intern "define-fun")
        ,(intern (name fn))
        (,@(map 'list
                #'(lambda (arg sort) (list (intern (name arg)) (intern (name sort))))
                (arguments fn) (argument-sorts fn)))
        ,(intern (name (return-sort fn)))
        ,(to-smt (definition fn)))
      `(,(intern "declare-fun")
        ,(intern (name fn))
        (,@(map 'list
                #'(lambda (x) (intern (name x))) (argument-sorts fn)))
        ,(intern (name (return-sort fn))))))

(defmethod to-smt ((lit literal))
  (value lit))

(defmethod to-smt (l) l) ; dump anything directly if no applicable method

(defun maybe-quote (name)
  (if (symbolp name)
      `(quote ,name)
      name))

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

(defmacro $int (name)
  `(int-variable ,(maybe-quote name)))

(defmacro $bool (name)
  `(bool-variable ,(maybe-quote name)))

(defmacro $string (name)
  `(string-variable ,(maybe-quote name)))

(defun function-declaration (name arg-sorts ret-sort &optional arguments definition)
  (make-instance 'function-declaration
                 :name (string name)
                 :arity (length arg-sorts)
                 :return-sort ret-sort
                 :argument-sorts arg-sorts
                 :arguments arguments
                 :definition definition))

(defmacro $function (name (&rest arg-sorts) ret-sort &optional args expression)
  (if (cl:not (null expression))
      `(function-declaration ,(maybe-quote name)
                             (list ,@arg-sorts)
                             ,ret-sort
                             (list ,@args)
                             ,expression)
      `(function-declaration ,(maybe-quote name)
                             (list ,@arg-sorts)
                             ,ret-sort)))

(defun $apply (fn &rest args)
  (make-instance 'expression
                 :name (name fn)
                 :sort (return-sort fn)
                 :arity (arity fn)
                 :children args
                 :child-sorts (argument-sorts fn)))

(defun quantifier-expression (type arguments sorts expression)
  (assert (= (length arguments) (length sorts)) nil "Arguments and sorts must have the same length.")
  (make-instance 'quantifier
                 :name type
                 :arguments arguments
                 :argument-sorts sorts
                 :children (list expression)
                 :child-sorts (list (sort expression))))

(defmacro $forall ((&rest bindings) expression)
  `(quantifier-expression "forall"
                          (map 'list #'identity (list ,@bindings))
                          (map 'list #'sort (list ,@bindings))
                          ,expression))

(defmacro $exists ((&rest bindings) expression)
  `(quantifier-expression "exists"
                          (map 'list #'identity (list ,@bindings))
                          (map 'list #'sort (list ,@bindings))
                          ,expression))

(defmethod to-smt ((expr quantifier))
  (assert (cl:or (string= (name expr) "forall")
              (string= (name expr) "exists")) nil
          "Quantifiers must be 'forall' or 'exists', but got: ~S" (name expr))
  `(,(intern (name expr)) (,@(map 'list
                                  #'(lambda (a s)
                                      (list
                                       (intern (name a))
                                       (intern (name s))))
                                  (arguments expr)
                                  (argument-sorts expr)))
    ,(to-smt (first (children expr)))))

;;
;; Expression operators
;;
(defun $+ (expr1 expr2)
  "Adds two expressions."
  (make-instance 'expression
                 :name "+"
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $- (expr1 expr2)
  "Subtracts two expressions."
  (make-instance 'expression
                 :name "-"
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $* (expr1 expr2)
  "Multiplies two expressions."
  (make-instance 'expression
                 :name "*"
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $< (expr1 expr2)
  "Compares two expressions."
  (make-instance 'expression
                 :name "<"
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $> (expr1 expr2)
  "Compares two expressions."
  (make-instance 'expression
                 :name ">"
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $= (expr1 expr2)
  "Compares two expressions."
  (make-instance 'expression
                 :name "="
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $not (expr)
  (make-instance 'expression
                 :name "not"
                 :sort *bool-sort*
                 :arity 1
                 :children (list expr)
                 :child-sorts (list (sort expr))))

(defun $and (&rest exprs)
  (make-instance 'expression
                 :name "and"
                 :sort *bool-sort*
                 :arity (length exprs)
                 :children exprs
                 :child-sorts (map 'list #'sort exprs)))

(defun $or (&rest exprs)
  (make-instance 'expression
                 :name "or"
                 :sort *bool-sort*
                 :arity (length exprs)
                 :children exprs
                 :child-sorts (map 'list #'sort exprs)))

(defun $xor (expr1 expr2)
  (make-instance 'expression
                 :name "xor"
                 :sort *bool-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $implies (expr1 expr2)
  (make-instance 'expression
                 :name "=>"
                 :sort *bool-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $iff (expr1 expr2)
  (and ($implies expr1 expr2) ($implies expr2 expr1)))

(defun $ite (exprb exprt expre)
  (assert (equal (name (sort exprt)) (name (sort expre))))
  (assert (equal (name (sort exprb)) (name *bool-sort*)))
  (make-instance 'expression
                 :name "ite"
                 :sort (sort exprt)
                 :arity 3
                 :children (list exprb exprt expre)
                 :child-sorts (list (sort exprb) (sort exprt) (sort expre))))

(defun $true ()
  (make-instance 'literal
                 :name "true"
                 :sort *bool-sort*
                 :value (intern "true")))

(defun $false ()
  (make-instance 'literal
                 :name "false"
                 :sort *bool-sort*
                 :value (intern "false")))
