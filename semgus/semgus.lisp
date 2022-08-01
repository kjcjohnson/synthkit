;;;
;;; Semgus.lisp - grammar + semantics
;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defclass semgus-problem ()
  ((grammar
    :initarg :grammar
    :initform (error "A grammar is required.")
    :reader grammar)
   (semantics
    :initarg :semantics
    :initform (error "Semantics are required.")
    :reader semantics)
   (specification
    :initarg :specification
    :initform (error "A specification is required.")
    :reader specification)
   (context
    :initarg :context
    :reader context)))

;;
;; Specification types
;;
(defclass io-specification ()
  ((examples
    :initarg :examples
    :initform (list)
    :reader examples)))

(defun add-example (spec input output)
  "Adds the given INPUT and OUTPUT as an example in SPEC."
  (declare (type io-specification spec))
  (push (cons input output) (slot-value spec 'examples)))

(defun example-input (example)
  "Gets the input value from the given example"
  (car example))

(defun example-output (example)
  "Gets the output value from the given example"
  (cdr example))

(defmacro with-example ((input-var output-var example) &body body)
  "Runs BODY with input-var and output-var bound to the example's input and output."
  `(let ((,input-var (example-input ,example))
         (,output-var (example-output ,example)))
     ,@body))

(defmacro do-examples ((input-var output-var spec) &body body)
  "Iterates over all examples in the input/output example specification."
  (declare (type io-specification spec))
  (let ((iter-var (gensym "ITER")))
    `(dolist (,iter-var (examples ,spec))
       (with-example (,input-var ,output-var ,iter-var)
         ,@body))))

(defclass formula-specification ()
  ((formula
    :initarg :formula
    :initform (smt:$true)
    :reader formula)
   (relation-name
    :initarg :relation-name
    :initform "rootprogram"
    :reader relation-name)))
