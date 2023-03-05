;;;;
;;;; Function events
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

;;;
;;; Function declarations and definitions
;;;
(defun com.kjcjohnson.synthkit.semgus.reader.user::rank
    (&key argument-sorts return-sort)
  "Creates a function rank"
  (declare (ignore argument-sorts return-sort))
  nil)

(defun com.kjcjohnson.synthkit.semgus.reader.user::declare-function (name &key rank)
  "Adds an auxiliary function declaration"
  (declare (ignore name rank))
  nil)

(defun com.kjcjohnson.synthkit.semgus.reader.user::define-function
    (name &key rank definition)
  "Adds an auxiliary function definition"
  (declare (ignore rank))
  (push (cons name definition) (semgus:auxiliary-functions semgus:*semgus-context*))
  ;; TODO: maybe, maybe not?
  (smt:set-function-definition name definition)
  nil)

