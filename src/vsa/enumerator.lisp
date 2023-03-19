;;;;
;;;; Enumerators over program nodes
;;;;
(in-package #:com.kjcjohnson.synthkit.vsa)

(deftype reset-delegate () '(function (program-node-enumerator) *))
(deftype move-next-delegate () '(function (program-node-enumerator) boolean))
(deftype current-delegate () '(function (program-node-enumerator) ast:program-node))

(defclass program-node-enumerator ()
  ((reset-fn :reader reset-fn
             :initarg :reset-fn
             :type reset-delegate
             :documentation "Call to reset the enumerator")
   (move-next-fn :reader move-next-fn
                 :initarg :move-next-fn
                 :type move-next-delegate
                 :documentation "Advances the enumerator to the next element")
   (current-fn :reader current-fn
               :initarg :current-fn
               :type current-delegate
               :documentation "Get the current element of the enumerator"))
  (:documentation "An enumerator over program nodes"))

(defun reset (e)
  "Resets the enumerator E"
  (declare (type program-node-enumerator e))
  (declare (optimize (speed 3)))
  (funcall (the reset-delegate (reset-fn e)) e))

(defun move-next (e)
  "Advances the enumerator E"
  (declare (type program-node-enumerator e))
  (declare (optimize (speed 3)))
  (funcall (the move-next-delegate (move-next-fn e)) e))

(defun current (e)
  "Gets the current element of enumerator E"
  (declare (type program-node-enumerator e))
  (declare (optimize (speed 3)))
  (funcall (the current-delegate (current-fn e)) e))

(defun enumerate-programs (function enumerator)
  "Calls FUNCTION with all elements from ENUMERATOR, a program node enumerator."
  (declare (type (function (t) *) function)
           (type (or program-node-enumerator program-node) enumerator))
  (declare (optimize (speed 3)))
  ;; Automatically convert from program nodes to enumerators for convenience
  (when (is-program-node? enumerator)
    (setf enumerator (enumerator enumerator)))
  ;; Let's not fetch the functions from the enumerator each time!
  (let ((move-next-fn (move-next-fn enumerator))
        (current-fn (current-fn enumerator)))
    (loop while (funcall (the move-next-delegate move-next-fn) enumerator)
          for program = (funcall (the current-delegate current-fn) enumerator)
          do (funcall function program))))

(defmacro do-programs ((var node-or-enumerator) &body body)
  "Loop over each program in ENUMERATOR, bound to VAR."
  `(flet ((body-fn (,var) ,@body))
     (enumerate-programs #'body-fn ,node-or-enumerator)))
