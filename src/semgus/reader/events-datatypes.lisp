;;;;
;;;; Datatypes and term types
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

;;;
;;; Term types
;;;
(defun com.kjcjohnson.synthkit.semgus.reader.user::declare-term-types (&rest types)
  "Adds the list of term types used in this problem"
  (loop for tt in types
        if (typep tt 'semgus:term-type)
          do (semgus:reset-term-type-constructors tt)
        else
          do (change-class tt 'semgus:term-type :constructors nil))
  (setf (semgus:term-types semgus:*semgus-context*) types))

(defun com.kjcjohnson.synthkit.semgus.reader.user::add-constructor
    (term-type &key operator children)
  "Adds a term type constructor to the context"
  (declare (type semgus:term-type term-type))
  (semgus:add-term-type-constructor term-type operator children))

;;;
;;; Datatypes
;;;
(defun com.kjcjohnson.synthkit.semgus.reader.user::declare-datatype (name &key arity)
  "Adds a datatype declaration (with no constructors yet)."
  (declare (ignore arity))
  (setf name (smt:ensure-identifier (smt:name name))) ; NAME comes in as a SORT
  (setf (smt:get-sort name) (make-instance 'smt:datatype :name name)))

(defun com.kjcjohnson.synthkit.semgus.reader.user::add-datatype-constructor
    (&key datatype name children)
  "Adds a constructor to a datatype."
  (let ((constructor (make-instance 'smt:datatype-constructor
                                    :name name
                                    :children children)))
    (unless (typep datatype 'smt:datatype)
      (error "Not a datatype: ~a" datatype))
    (smt:add-datatype-constructor datatype constructor)))
