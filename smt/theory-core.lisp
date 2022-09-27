;;;;
;;;; Core theory functions
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defun core-true ()
  "Core true"
  t)

(defun core-false ()
  "Core false"
  nil)

(defun core-not (other)
  "Core not function"
  (declare (type boolean other))
  (not other))

(defun core-or (left right)
  "Core or function"
  (declare (type boolean left right))
  (or left right))

(defun core-and (left right)
  "Core and function"
  (declare (type boolean left right))
  (and left right))

(defun core-=> (left right)
  "Core implication"
  (core-or (core-not left) right))

(defun core-xor (left right)
  "Core exclusive or"
  (core-and
   (core-or left right)
   (core-or (core-not left) (core-not right))))

(defun core-= (left right)
  "Core equality"
  (assert (eql (get-constant-type left) (get-constant-type right))
          (left right)
          "Cannot compare arguments of different types: ~a and ~a"
          left right)
  (etypecase left
    (number (= left right))
    (boolean (eql left right))
    (string (string= left right))
    (bit-vector (equal left right))
    (datatype-instance (datatype= left right))))

(defun core-distinct (left right)
  "Core distinct"
  (core-not (core-= left right)))

(defun core-ite (condition consequence alternative)
  "Core if-then-else"
  (declare (type boolean condition))
  (assert (eql (get-constant-type consequence) (get-constant-type alternative))
          (consequence alternative)
          "ITE branches must have the same types: ~a and ~a"
          consequence alternative)
  (if condition
      consequence
      alternative))
