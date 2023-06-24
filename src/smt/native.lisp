;;;;
;;;; Native SMT expressions - for adding native CL code to SMT expressions
;;;;    (with caveats)
(in-package #:com.kjcjohnson.synthkit.smt)

;;;
;;; We define a native sort - which can be any CL type. Use with prudence!
;;;
(defclass native-sort (sort)
  ()
  (:default-initargs :name "native")
  (:documentation "An internal sort representing any CL type"))

(defparameter *native-sort* (make-instance 'native-sort) "The native sort singleton")

;;;
;;; Two classes of natives - expressions (funcallables) and literals (values)
;;;
(defclass native-expression (expression)
  ()
  (:documentation "A native Lisp SMT expression"))

(defmethod to-smt ((expression native-expression) &key pprint)
  "Throw an error if we attempt to convert a native expression to SMT forms"
  (if pprint
      "[NATIVE]"
      (error "Attempt to convert native expression to SMT form: ~s" expression)))

(defclass native-literal (native-expression literal)
  ((value :reader native-value
          :initarg :value
          :documentation "A CL value for this literal"))
  (:default-initargs :name "native-literal" :sort *native-sort*)
  (:documentation "A literal CL value in an SMT expression"))

(defun make-native-literal (value)
  "Creates a native literal SMT expression"
  (make-instance 'native-literal :value value))

(defun native-literal? (expression)
  "Checks if EXPRESSION is a native literal"
  (typep expression 'native-literal))

(defmethod copy-node ((node native-literal) &key)
  "Copies a native literal node"
  (make-native-literal (value node)))

(defmethod to-smt ((expression native-literal) &key pprint)
  (if pprint
      (format nil "[NATIVE ~a]" (native-value expression))
      (call-next-method)))

;;;
;;; For debugging - insert break statements
;;;
(defclass native-break (native-expression)
  ((condition :reader native-break-condition
              :initarg :condition
              :documentation "Code to call - if true, breaks"))
  (:default-initargs :name "native-break"
                     :sort *bool-sort*
                     :arity 0
                     :children nil
                     :child-sorts nil
                     :condition (constantly t))
  (:documentation "Causes a CL break"))

(defun native-break? (expression)
  "Checks if EXPRESSION is a native break"
  (typep expression 'native-break))

(defun make-native-break (&optional condition)
  "Creates a break expression, optionally breaking when CONDITION is T. CONDITION
should be a function that takes no arguments and returns T or NIL."
  (make-instance 'native-break :condition condition))

(defmethod copy-node ((node native-break) &key)
  "Copies a native break node"
  (make-native-break (native-break-condition node)))

(defmethod to-smt ((expression native-break) &key pprint)
  (if pprint
      (format nil "[BREAK]")
      (call-next-method)))
