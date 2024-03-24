;;;;
;;;; The writer protocol for SemGuS
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defgeneric write-problem (stream problem writer)
  (:documentation "Writes PROBLEM to STREAM with the format designator WRITER"))

(defgeneric write-program (stream names programs writer)
  (:documentation "Writes PROGRAMS (named by NAMES) to STREAM with the format
designator WRITER. NAMES and PROGRAMS can either be a single name/program, or each a
sequence of names and associated ASTs."))
