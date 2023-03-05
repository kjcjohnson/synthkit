;;;;
;;;; Term events
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

;;;
;;; Term handling
;;;
(defun com.kjcjohnson.synthkit.semgus.reader.user::term (term)
  "Wraps a term"
  term)

(defun com.kjcjohnson.synthkit.semgus.reader.user::application
    (name &key argument-sorts arguments return-sort)
  "Creates a function application term"
  (make-instance 'smt::expression
                 :name name
                 :sort return-sort
                 :arity (length arguments)
                 :children arguments
                 :child-sorts argument-sorts))

(defun com.kjcjohnson.synthkit.semgus.reader.user::variable (name &key sort)
  "Creates a variable term"
  (smt:variable name sort))

(defun com.kjcjohnson.synthkit.semgus.reader.user::exists
    (&key bindings binding-sorts child)
  "Creates an existential quantifier"
  (smt::quantifier-expression "exists" bindings binding-sorts child))

(defun com.kjcjohnson.synthkit.semgus.reader.user::forall
    (&key bindings binding-sorts child)
  "Creates a universal quantifier"
  (smt::quantifier-expression "forall" bindings binding-sorts child))

(defun com.kjcjohnson.synthkit.semgus.reader.user::match (&key term binders)
  "Creates a match term"
  (declare (ignore term binders))
  ;; currently not supported
  nil)

(defun com.kjcjohnson.synthkit.semgus.reader.user::binder
    (&key operator arguments child)
  "Creates a binder for a match clause"
  (declare (ignore operator arguments child))
  ;; currently not supported
  nil)

(defun com.kjcjohnson.synthkit.semgus.reader.user::lambda (&key arguments body)
  (smt::make-lambda-binder arguments body))
