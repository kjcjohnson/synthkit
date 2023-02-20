;;;;
;;;; Protocol for SemGuS verifiers
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defgeneric verify-program (verifier semgus-problem program &key produce-cex)
  (:documentation "Verifies PROGRAM against SEMGUS-PROBLEM.
Returns :VALID, :INVALID, or :UNKNOWN as the first value, and
optionally a counter-example (if :INVALID) as the second value."))
