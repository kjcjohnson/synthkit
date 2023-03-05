;;;;
;;;; Theory functions for Ints
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defsmtfun "+" :ints (&rest numbers)
  "Integer addition"
  (apply #'+ numbers))

(defsmtfun "-" :ints (&rest numbers)
  "Integer negation and subtraction"
  (apply #'- numbers))

(defsmtfun "<" :ints (number &rest numbers)
  "Integer less-than"
  (apply #'< number numbers))

(defsmtfun ">" :ints (number &rest numbers)
  "Integer greater-than"
  (apply #'> number numbers))

(defsmtfun "*" :ints (number &rest numbers)
  "Integer multiplication"
  (apply #'* number numbers))
