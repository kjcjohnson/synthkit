;;;
;;; Various useful utilities
;;;
(in-package #:com.kjcjohnson.synthkit.utilities)

(defun choose-uniformly-at-random (&rest options)
  "Chooses and returns one of OPTIONS uniformly at random."
  (nth (random (length options)) options))
