;;;;
;;;; Base specification class
;;;;
(in-package #:com.kjcjohnson.synthkit.specification)

(defclass specification ()
  ()
  (:documentation "A generic specification for a problem"))

(defclass relational-specification (specification)
  ((expression :initarg :expression
               :reader expression
               :documentation "SMT expression for this specification")
   (descriptors :initarg :descriptors
                :reader descriptors
                :documentation "Descriptors used in this specification"))
  (:documentation "A specification from an SMT expression"))

;; TODO: do inputs/outputs need sorts?
(defclass universal-specification (relational-specification)
  ((input-symbols :reader input-symbols
                  :initarg :input-symbols
                  :type (vector symbol)
                  :documentation "Vector of input symbols used in this formula")
   (output-symbols :reader output-symbols
                   :initarg :output-symbols
                   :type (vector symbol)
                   :documentation "Vector of output symbols used in this formula")
   (constraint :reader constraint
               :initarg :constraint
               :type smt::expression
               :documentation "Expression (using i/o symbols) specifying behavior"))
  (:documentation "A specification written in the form X.Sem(t,i,o) <=> Phi(i,o)."))

(defun is-universal? (spec)
  "Checks if SPEC is a universal specification"
  (typep spec 'universal-specification))

;; TODO: do inputs/outputs need sorts?
(defclass existential-specification (relational-specification)
  ((input-symbols :reader input-symbols
                  :initarg :input-symbols
                  :type (vector symbol)
                  :documentation "Vector of input symbols used in this formula")
   (output-symbols :reader output-symbols
                   :initarg :output-symbols
                   :type (vector symbol)
                   :documentation "Vector of output symbols used in this formula")
   (constraint :reader constraint
               :initarg :constraint
               :type smt::expression
               :documentation "Expression (using i/o symbols) specifying behavior"))
  (:documentation "A specification written in the form X.Sem(t,i,o)^Phi(i,o)."))

(defun is-existential? (spec)
  "Checks if SPEC is an existential specification"
  (typep spec 'existential-specification))
