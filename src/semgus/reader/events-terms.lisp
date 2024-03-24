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
  (make-instance 'smt:application
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
  (let ((datatype (smt:sort term)))
    (unless (or (semgus:is-term-type? datatype)
                (smt:is-datatype? datatype))
      (error "Not a datatype or term-type: ~a" datatype))
    (smt:make-match-grouper
     term
     (map 'vector #'(lambda (bp)
                      (assert (eql (first bp) :binder-proxy))
                      (apply #'smt:make-match-binder datatype (rest bp)))
          binders))))

(defun com.kjcjohnson.synthkit.semgus.reader.user::binder
    (&key operator arguments child)
  "Creates a binder for a match clause"
  ;; We don't have enough information yet to resolve operator -> constructor
  (list :binder-proxy operator arguments child))

(defun com.kjcjohnson.synthkit.semgus.reader.user::lambda (&key arguments body)
  (smt::make-lambda-binder arguments body))
