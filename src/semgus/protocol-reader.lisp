;;;;
;;;; The Reader protocol (well, actually, just something to get loading to be happy)
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defgeneric process-chcs-for-relational-semantics (context)
  (:documentation "Processes CHCs out of the given SemGuS context"))

(defgeneric read-problem-from-stream (stream context)
  (:documentation "Reads a SemGuS problem from STREAM"))

(defgeneric derive-specification (context)
  (:documentation "Derives a specification from the given context"))

;;;
;;; Processor hooks
;;;
(defgeneric on-context-load (processor context)
  (:documentation "Called when a SemGuS context is loaded"))

(defgeneric on-operationalization-load (processor op-sem-map desc-map context)
  (:documentation "Called when operational semantics are loaded"))

(defgeneric on-problem-load (processor problem)
  (:documentation "Called when a SemGuS problem is loaded"))

(defmethod on-context-load (processor context)
  "Default no-op context load hook"
  (declare (ignore processor context))
  nil)

(defmethod on-operationalization-load (processor op-sem-map desc-map context)
  "Default no-op operationalization load hook"
  (declare (ignore processor op-sem-map desc-map context))
  nil)

(defmethod on-problem-load (processor problem)
  "Default no-op problem load hook"
  (declare (ignore processor problem))
  nil)

;;;
;;; Processor hook implementation
;;;
(defvar *load-processors* nil "Processors that are called on loading")

(defun register-load-processor (processor)
  "Registers PROCESSOR to be called on context load"
  (pushnew processor *load-processors*))

(defun call-context-load-processors (context)
  "Calls all registered context load hooks on CONTEXT"
  (dolist (processor *load-processors*)
    (on-context-load processor context))
  context)

(defun call-operationalization-load-processors (op-sem-map desc-map context)
  "Calls all registered operationalization load hooks on OP-SEM-MAP, DESC-MAP, CONTEXT"
  (dolist (processor *load-processors*)
    (on-operationalization-load processor op-sem-map desc-map context))
  context)

(defun call-problem-load-processors (problem)
  "Calls all registered problem load hooks on PROBLEM"
  (dolist (processor *load-processors*)
    (on-problem-load processor problem))
  problem)
