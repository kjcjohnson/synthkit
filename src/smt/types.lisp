;;;;
;;;; Mapping of SMT types to Lisp types
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defun get-constant-type (thing)
  "Gets a consistent SMT type from a constant value."
  (typecase thing
    (number 'number)
    (boolean 'boolean)
    (string 'string)
    (bit-vector 'bit-vector)
    (datatype-instance 'datatype-instance)
    (otherwise (type-of thing))))
