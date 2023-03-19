;;;;
;;;; Protocol for computing hash codes of SMT objects
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defgeneric hash-code (obj)
  (:documentation "Computes a hash code for OBJ")
  (:method (obj) (sxhash obj)))
