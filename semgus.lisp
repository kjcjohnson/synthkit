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
    :reader specification)))
