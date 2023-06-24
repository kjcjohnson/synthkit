;;;;
;;;; Counters for various program execution statistics
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

;;;
;;; Counts number of programs executed
;;;
(defvar *execution-counter* 0 "Count of program executions - NOT unique programs")

;;;
;;; Counters for program types considered
;;;
(defvar *candidate-concrete-programs* 0 "Count of candidate concrete programs
considered. What this specifically means is up to the individual algorithm.")

(defvar *candidate-partial-programs* 0 "Count of candidate partial programs
considered. What this specifically means is up to the individual algorithm.")

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
