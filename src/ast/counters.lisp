;;;;
;;;; Counters for various program execution statistics
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

;;;
;;; Counts number of programs executed
;;;
(defvar *execution-counter* 0 "Count of program executions - NOT unique programs")

;;;
;;; Checkpoints and tracing for notable events in a synthesis run's lifetime
;;;
(defvar *program-trace-stream* nil "Stream to write program traces to")

(defvar *checkpoint-times* nil "Plist of times when checkpoints were reached

*CHECKPOINT-TIMES* is a plist mapping a checkpoint atom (string, symbol, etc.) to a
time in seconds from the beginning of the synthesis run. Synthesis algorithms should
call ``ADD-CHECKPOINT`` at appropriate times to mark a new checkpoint in the execution.
")

(defun add-checkpoint (checkpoint)
  "Adds a new checkpoint to the current synthesis execution

CHECKPOINT can be any indicator that can be serialized to an execution engine."
  (setf (getf *checkpoint-times* checkpoint)
        (/ (get-internal-real-time) internal-time-units-per-second))
  (when *program-trace-stream*
    (format *program-trace-stream* "~&=========== [~a] ===========~%" checkpoint)))

(defun clear-all-checkpoints ()
  "Clears all checkpoints"
  (setf *checkpoint-times* nil))

(declaim (inline trace-program))
(defun trace-program (program)
  "Report PROGRAM to the program trace stream"
  (when *program-trace-stream*
    (format *program-trace-stream* "~&~a~%" program)))

;;;
;;; Counters for program types considered
;;;
(defvar *candidate-concrete-programs* 0 "Count of candidate concrete programs
considered. What this specifically means is up to the individual algorithm.")

(defvar *candidate-partial-programs* 0 "Count of candidate partial programs
considered. What this specifically means is up to the individual algorithm.")

(defvar *concrete-candidates-by-size* nil "Plist of size --> candidate count")
;;;
;;; Pruning counters
;;;
(defvar *prune-candidate-counter* 0 "Count of programs that are eligible for pruning,
under the current synthesis procedure's rules. These are all programs that can be
checked if prunable, not programs that are actually safe to prune.")

(defvar *prune-attempt-counter* 0 "Count of programs that were attempted to be pruned;
that is, programs that were either pruned or determined to not be safe to prune. In
other words, the subset of candidate programs that had a do/don't prune decision.")

(defvar *prune-success-counter* 0 "Count of programs that were successfully pruned.")
