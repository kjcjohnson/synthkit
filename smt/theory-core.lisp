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
  (assert (or (subtypep (type-of left) (type-of right))
              (subtypep (type-of right) (type-of left)))
          (left right)
          "Cannot compare arguments of different types: ~a and ~a"
          left right)
  (etypecase left
    (number (= left right))
    (boolean (eql left right))
    (bit-vector (equal left right))))

(defun core-distinct (left right)
  "Core distinct"
  (core-not (core-= left right)))

(defun core-ite (condition consequence alternative)
  "Core if-then-else"
  (declare (type boolean condition))
  (assert (or (subtypep (type-of consequence) (type-of alternative))
              (subtypep (type-of alternative) (type-of consequence)))
          (consequence alternative)
          "ITE branches must have the same types: ~a and ~a"
          consequence alternative)
  (if condition
      consequence
      alternative))
