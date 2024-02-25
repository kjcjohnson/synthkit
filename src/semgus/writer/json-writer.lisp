;;;;
;;;; JSON writer for SemGuS problems
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.writer)

(defmethod semgus:write-problem (stream problem (writer (eql :json)))
  "Writes a SemGuS problem as a JSON stream"
  (error "Not yet implemented")
  problem)
