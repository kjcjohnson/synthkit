;;;;
;;;; Theory functions for Ints
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defun ints-+ (&rest numbers)
  "Integer addition"
  (apply #'+ numbers))

(defun ints-- (&rest numbers)
  "Integer negation and subtraction"
  (apply #'- numbers))

(defun ints-< (number &rest numbers)
  "Integer less-than"
  (apply #'< number numbers))
