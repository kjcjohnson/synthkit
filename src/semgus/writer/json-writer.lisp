;;;;
;;;; JSON writer for SemGuS problems
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.writer)

(defmethod semgus:write-problem (stream problem (writer (eql :json)))
  "Writes a SemGuS problem as a JSON stream"

  (jzon:with-writer* (:stream stream :pretty t)
    (jzon:with-array*
      ;; 1. METADATA

      ;; 2. AUXILIARIES

      ;; 3. TERM-TYPES
      (loop for tt in (semgus:term-types (semgus:context problem))
            for ev = (make-instance 'declare-term-type-event :name tt)
            do (jzon:write-value* ev))

      (loop for tt in (semgus:term-types (semgus:context problem))
            for ev = (make-instance 'define-term-type-event :name tt)
            do (jzon:write-value* ev))

      ;; N-3. CHCS
      (loop for chc in (semgus:chcs (semgus:context problem))
            for ev = (make-instance 'chc-event
                                    :head (chc:head chc)
                                    :body (chc:body chc))
            do (jzon:write-value* ev))

      ;; N-2. SYNTH-FUN

      ;; N-1. CONSTRAINTS

      ;; N. CHECK-SYNTH
      (jzon:write-value* (make-instance 'check-synth-event))))

    ;; NOTE: Technically, auxiliaries could be anywhere. But 🤷‍♂️

  problem)
