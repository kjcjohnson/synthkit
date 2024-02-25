;;;;
;;;; The writer protocol for SemGuS
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defgeneric write-problem (stream problem writer)
  (:documentation "Writes PROBLEM to STREAM with the format designator WRITER"))
