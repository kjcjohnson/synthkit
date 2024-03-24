;;;;
;;;; Handling for match constructs
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

;;;
;;; PATTERNS - what we match on and what variables get bound
;;;
(defclass match-pattern ()
  ((datatype :initarg :datatype
             :reader match-pattern-datatype
             :documentation "The datatype this match pattern is over"))
  (:documentation "A pattern for a match binder"))

(defclass match-pattern-datatype (match-pattern)
  ((constructor :initarg :constructor
                :reader match-pattern-constructor
                :documentation "The datatype constructor this pattern is over")
   (variables :initarg :variables
              :reader match-pattern-variables
              :type vector
              :documentation "Vector of variables for this pattern"))
  (:documentation "A match pattern matching a datatype constructor"))

(defclass match-pattern-singleton (match-pattern)
  ((variable :initarg :variable
             :reader match-pattern-variable
             :documentation "The variable this pattern matches"))
  (:documentation "A match pattern matching a single variable"))

;;;
;;; BINDERS - a pattern and a term for when the pattern is matched
;;;
(defclass match-binder (smt-node)
  ((pattern :reader match-pattern
            :initarg :pattern
            :type match-pattern
            :documentation "The pattern this binder matches")
   (term :reader match-term
         :initarg :term))
  (:documentation "A match binding. Has a pattern and a term, which, if the match is
successful on the pattern, binds the variables in pattern in the term."))

(defun make-match-binder (datatype constructor-name arguments child)
  "Creates a match binder. If CONSTRUCTOR is NIL, then arguments is a single symbol"
  (if (null constructor-name)
      (progn
        (when (listp arguments) (error "Got list of variables for singleton match"))
        (let ((pattern (make-instance 'match-pattern-singleton
                                      :variable arguments
                                      :datatype datatype)))
          (make-instance 'match-binder :pattern pattern :term child)))
      (let ((constructor (lookup-datatype-constructor datatype constructor-name)))
        (unless (= (length (children constructor)) (length arguments))
          (error "Mismatch in constructor/variables arity"))
        (let ((pattern (make-instance 'match-pattern-datatype
                                      :constructor constructor
                                      :variables (u:ensure-vector arguments)
                                      :datatype datatype)))
          (make-instance 'match-binder
                         :pattern pattern
                         :term child)))))



;;;
;;; GROUPER - for holding a term to match and a set of binders
;;;
(defclass match-grouper (term)
  ((child :reader match-child
          :initarg :child
          :type term
          :documentation "Term being matched")
   (binders :reader match-binders
            :initarg :binders
            :type (vector match-binder)
            :documentation "Vector of match binders"))
  (:default-initargs :name "match")
  (:documentation "A match term"))

(defun make-match-grouper (term binders)
  "Creates a match grouper on TERM with BINDERS"
  (when (zerop (length binders)) (error "No binders for match grouper"))
  (let ((child-sort (sort term))
        (match-sort (sort (match-term (elt binders 0)))))
    (declare (ignore child-sort))
    ;; Challenge: we need SemGuS term types to act like datatypes
    ;;(unless (typep child-sort 'datatype)
    ;;  (error "Not a datatype sort: ~a" child-sort))

    ;; TODO: validate that binders:
    ;;   - have real constructors (for the same datatype)
    ;;   - are complete (i.e., exhaustive or have a singleton pattern)
    ;;   - have the same return sort
    (make-instance 'match-grouper :child term
                                  :sort match-sort
                                  :binders (u:ensure-vector binders))))
