;;;;
;;;; Variables for AST and execution control
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

;;;
;;; Allows exiting a program early
;;;
(defvar *program-execution-exit-hook*)
(defun abort-program-execution ()
  (funcall *program-execution-exit-hook*))

;;;
;;; Handles simple unbounded-recursion detection
;;;
(defvar *self-recursion-counter* 0
  "Counts recursion depth to find probably-non-terminating programs.")
(defparameter *self-recursion-limit* 200)
(declaim (type fixnum *self-recursion-counter* *self-recursion-limit*))

;;;
;;; Debugging controls
;;;
(defvar *exe-debug* nil "Set to T when execution debugging is triggered")
(defvar *exe-level* 0 "Level of nested program execution")
(defvar *exe-debug-match* nil "If non-nil, must be a string that, when contained in the
serialization of a program node, triggers ``*EXE-DEBUG*`` to be set to T.")
(declaim (type (or null string) *exe-debug-match*))

;;;
;;; Stores the root input state
;;;
(defvar *root-input-state* nil "The initial input state passed to EXECUTE-PROGRAM")
(defvar *root-input-descriptor* nil "The initial descriptor passed to EXECUTE-PROGRAM")

;;;
;;; Control variables
;;;
(defvar *use-program-compiler* nil "Whether or not to use the program compiler")
