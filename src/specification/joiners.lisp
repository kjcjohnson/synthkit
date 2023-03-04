;;;;
;;;; Compound specifications
;;;;
(in-package #:com.kjcjohnson.synthkit.specification)

(defclass compound-specification (specification)
  ((components :initarg :components
               :accessor components
               :documentation "Components making up this compound specification"))
  (:documentation "A specification that encapulates other component specifications"))

(defclass intersection-specification (compound-specification)
  ()
  (:documentation "A satisfying program will be in the intersection of all programs
satisfying each of the component specifications."))

(defclass union-specification (compound-specification)
  ()
  (:documentation "A satisfying program will be in the union of all programs satisfying
each of the component specifications."))

(defgeneric leaf-specification-types (spec)
  (:documentation "Returns a list of the leaf specifications in SPEC")
  (:method ((spec compound-specification))
    (reduce #'union (map 'list #'leaf-specification-types (components spec))))
  (:method ((spec specification))
    (list (class-name (class-of spec)))))

(defun is-only? (spec type)
  "Checks if the given SPEC only has specifications of type TYPE"
  (every (a:rcurry #'subtypep type) (leaf-specification-types spec)))

(defgeneric compound-specification-types (spec)
  (:documentation "Returns a list of the compound joiner types in SPEC")
  (:method ((spec compound-specification))
    (reduce #'union (map 'list #'compound-specification-types (components spec))
            :initial-value (class-name (class-of spec))))
  (:method ((spec specification))
    (list)))

(defun with-only? (spec type)
  "Checks if SPEC only has compound specification joiners of type TYPE"
  (every (a:rcurry #'subtypep type) (compound-specification-types spec)))

(defun with-only-intersection? (spec)
  "Checks if SPEC only uses intersections as joiners"
  (with-only? spec 'intersection-specification))

(defun with-only-union? (spec)
  "Checks if SPEC only uses unions as joiners"
  (with-only? spec 'union-specification))
