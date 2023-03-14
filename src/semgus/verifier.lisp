;;;;
;;;; Protocol for SemGuS verifiers
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defgeneric verify-program (verifier specification semgus-problem program
                            &key produce-cex)
  (:documentation "Verifies PROGRAM against SEMGUS-PROBLEM.
Returns :VALID, :INVALID, or :UNKNOWN as the first value, and
optionally a counter-example (if :INVALID) as the second value."))

(defgeneric verifier-for-specification (specification semgus-problem &key produce-cex)
  (:documentation "Chooses a verifier that can handle verifying the specification"))

(define-condition unknown-verifier-result (error)
  ()
  (:documentation "Signaled when a verifier gets an inconclusive result"))

(defun check-program (semgus-problem program &key specification (on-unknown :error))
  "Checks if PROGRAM satisfies the specification in SEMGUS-PROBLEM. Returns T if PROGRAM
satisfies the specification in SEMGUS-PROBLEM, NIL otherwise. If PROGRAM is unable to
be verified, signals an error of type UNKNOWN-VERIFIER-RESULT."
  (declare (type (member :error :valid :invalid)))
  (let ((verifier (verifier-for-specification (or specification
                                                  (specification semgus-problem))
                                              semgus-problem
                                              :produce-cex nil)))
    (ecase (verify-program verifier (or specification (specification semgus-problem))
                           semgus-problem program :produce-cex nil)
      (:valid t)
      (:invalid nil)
      (:unknown (ecase on-unknown
                  (:error (error 'unknown-verifier-result))
                  (:valid :valid)
                  (:invalid :invalid))))))
