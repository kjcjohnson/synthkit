;;;;
;;;; The Reader protocol (well, actually, just something to get loading to be happy)
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defgeneric process-chcs-for-relational-semantics (context)
  (:documentation "Processes CHCs out of the given SemGuS context"))

