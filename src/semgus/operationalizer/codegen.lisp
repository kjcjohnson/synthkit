;;;;
;;;; Operationalizer code generator
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.operationalizer)

(defclass codegen-data ()
  ((input-symbols
    :initarg :input-symbols
    :accessor %codegen-input-symbols
    :documentation "Input variables for relation.")
   (output-symbols
    :initarg :output-symbols
    :accessor %codegen-output-symbols
    :documentation "Output variables for relation.")
   (auxiliary-symbols
    :initarg :auxiliary-symbols
    :accessor %codegen-auxiliary-symbols
    :documentation "Auxiliary variables in a relation.")
   term-symbols
   ordering
   node-table
   index-table
   semantics-name
   block-name
   input-state-symbol
   child-semantic-functions
   ))

(defvar *data* nil "Code generation data context")

(defmacro with-codegen-data ((&rest args
                              &key
                                input-symbols
                                output-symbols
                                auxiliary-symbols
                              &allow-other-keys) &body body)
  "Initializes the data object."
  (declare (ignorable input-symbols output-symbols auxiliary-symbols))
  `(let ((*data* (make-instance 'codegen-data ,@args)))
     ,@body))

(defmacro with-data-slots (&body body)
  "Gives access to codegen data slots."
  `(with-slots (input-symbols
                output-symbols
                auxiliary-symbols
                term-symbols
                ordering
                node-table
                index-table
                semantics-name
                block-name
                input-state-symbol
                child-semantic-functions)
       *data*
     ,@body))

(defun %codegen-make-state (&key formals (actuals formals))
  "Generates a temporary SMT state. FORMALS should be a list of symbols for the 
state variable names, and ACTUALS a list of in-scope variable names to assign
to the state variables. If ACTUALS is not provided, defaults to FORMALS."
  (declare (ignore actuals))
  )

(defun %codegen-block-return (&key value (successful t))
  "Generates a block return."
  (with-data-slots
    `(return-from ,block-name (values ,value ,successful))))

(defun %codegen-function ()
  "Generates the wrapper lambda and block for a semantic function"
  (with-data-slots
    (setf block-name (gensym "BLOCK")
          input-state-symbol (gensym "INPUT")
          child-semantic-functions (map 'list #'(lambda (x)
                                                  (declare (ignore x))
                                                  (gensym "CS"))
                                        term-symbols))
    `(lambda (,input-state-symbol ,@child-semantic-functions)
       (declare (ignorable ,input-state-symbol ,@child-semantic-functions))
       (declare (optimize (speed 3)))
       (block ,block-name
         ,@nil))))

(defun %size-at-least? (tree size)
  "Returns if TREE has at least SIZE cons cells in it."
  (let ((count 0))
    (labels ((inc-and-check ()
               (when (>= (incf count) size)
                 (return-from %size-at-least? t)))
             
             (traversal (subtree)
               (when (consp subtree)
                 (inc-and-check)
                 (traversal (car subtree))
                 (traversal (cdr subtree)))))
      (traversal tree))))

(defparameter *sub-compile-threshold* 2000)

(defun %codegen-expression-node (expression input-vars output-vars)
  "Generates code for an expression node."
  (let* ((exp-code
           #|(operationalize-smt-expression expression
           input-vars
           output-vars))|# ; KNOTE: ???
           (%operationalize-expression expression input-vars output-vars))
         (exp-call
           (if (%size-at-least? exp-code *sub-compile-threshold*)
               (let (comp-fn)
                 (format *trace-output* "~&; Sub-compile...~%")
                 (let ((sub-lambda
                         `(lambda ,input-vars
                            (declare (optimize (speed 3)))
                            (let ,output-vars
                              (let ((res-val ,exp-code))
                                (list res-val ,@output-vars))))))
                   ;;(format t "~s~%" sub-lambda)
                   (setf comp-fn (compile nil sub-lambda)))
                 `(let ((res-list (funcall ,comp-fn ,@input-vars)))
                    ,@(loop
                        for i from 1 to (length output-vars)
                        for ovar in output-vars
                        collecting `(setf ,ovar (elt res-list ,i)))
                    (car res-list)))
               exp-code)))
    `(let ((eval-result ,exp-call))
       #+sbcl
       (declare (sb-ext:muffle-conditions
                 sb-ext:code-deletion-note))
       (unless eval-result
         ,(%codegen-block-return :successful nil)))))
