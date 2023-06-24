;;;;
;;;; Meta-level events
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

(defun com.kjcjohnson.synthkit.semgus.reader.user::set-info (name &optional prop)
  "Set info...not well implemented"
  (setf (gethash name (semgus:metadata semgus:*semgus-context*)) prop))
