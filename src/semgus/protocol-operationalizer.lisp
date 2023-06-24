;;;;
;;;; Protocol for the operationalizer
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defgeneric operationalize-chc (chc smt-context semgus-context)
  (:documentation "Operationalizes a CHC with the given SMT and SemGuS contexts.
Returns a calling card object as the result."))
