;;;;
;;;; Utility event functions
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

(defun com.kjcjohnson.synthkit.semgus.reader.user::list (&rest stuff)
  "Creates a list"
  stuff)

(defun com.kjcjohnson.synthkit.semgus.reader.user::identifier (&rest indices)
  "Creates an identifier"
  (smt:ensure-identifier indices))

(defun com.kjcjohnson.synthkit.semgus.reader.user::sort (identifier)
  "Creates a sort"
  (smt:ensure-sort identifier))

(defparameter com.kjcjohnson.synthkit.semgus.reader.user::nil nil
  "Nil can appear as an operator in productions that are an NT-to-NT production.")
