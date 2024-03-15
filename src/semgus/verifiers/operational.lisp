;;;;
;;;; An operational verifier
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.verifiers)

(defclass operational-verifier ()
  ()
  (:documentation "A verifier that executes programs to verify. Only handles inductive
specifications and cannot produce counter-examples."))
(defparameter *operational-verifier-instance* (make-instance 'operational-verifier))

(defmethod semgus:verifier-for-specification
    ((spec spec:inductive-specification) semgus-problem &key produce-cex)
  (if produce-cex
      (call-next-method)
      *operational-verifier-instance*))

(defmethod semgus:verify-program
    ((verifier operational-verifier) (spec spec:inductive-specification)
     semgus-problem program &key produce-cex)
  "Operationally verifies a program."
  (assert (not produce-cex))
  (multiple-value-bind (res valid)
      (ast:execute-program (semgus:semantics semgus-problem)
                           (spec:descriptor spec)
                           program
                           (spec:input-state spec))
    (if valid
        (if (funcall (spec:predicate spec) res)
            :valid
            :invalid)
        :unknown)))

(defmethod semgus:verify-program
    ((verifier operational-verifier) (spec spec:io-specification)
     semgus-problem program &key produce-cex)
  "Operationally verifies a program."
  (assert (not produce-cex))
  (assert (typep spec 'spec:inductive-specification))
  (if (smt:state= (ast::execute-program (semgus:semantics semgus-problem)
                                        (spec:descriptor spec)
                                        program
                                        (spec:input-state spec))
                  (spec:output-state spec))
      :valid
      :invalid))
