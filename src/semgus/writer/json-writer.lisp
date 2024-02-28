;;;;
;;;; JSON writer for SemGuS problems
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.writer)

(defmethod semgus:write-problem (stream problem (writer (eql :json)))
  "Writes a SemGuS problem as a JSON stream"
  (let ((ctx (semgus:context problem)))
    (jzon:with-writer* (:stream stream :pretty t)
      (jzon:with-array*
        ;; 1. METADATA

        ;; 2. AUXILIARIES
        (loop for (name rank defn) in (semgus:auxiliary-functions ctx)
              for ev = (make-instance 'declare-function-event :name name :rank rank)
              do (jzon:write-value* ev))

        ;; 3. TERM-TYPES
        (loop for tt in (semgus:term-types ctx)
              for ev = (make-instance 'declare-term-type-event :name tt)
              do (jzon:write-value* ev))

        (loop for tt in (semgus:term-types ctx)
              for ev = (make-instance 'define-term-type-event :name tt)
              do (jzon:write-value* ev))

        ;; N-3. CHCS
        (loop for chc in (reverse (semgus:chcs ctx))
              for ev = (make-instance 'chc-event :chc chc)
              do (jzon:write-value* ev))

        ;; N-2. SYNTH-FUN
        (jzon:write-value* (make-instance 'synth-fun-event
                                          :name (semgus:term-name ctx)
                                          :term-type (semgus:term-type ctx)
                                          :grammar (semgus:grammar ctx)))

        ;; N-1. CONSTRAINTS
        (loop for constraint in (semgus:constraints ctx)
              for ev = (make-instance 'constraint-event :constraint constraint)
              do (jzon:write-value* ev))

        ;; N. CHECK-SYNTH
        (jzon:write-value* (make-instance 'check-synth-event))

        (jzon:write-value* (make-instance 'end-of-stream-event)))))

    ;; NOTE: Technically, auxiliaries could be anywhere. But 🤷‍♂️

  problem)
