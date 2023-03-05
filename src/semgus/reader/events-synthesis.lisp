;;;;
;;;; Synthesis events
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

;;;
;;; Synthfun and Grammar handling
;;;
(defun com.kjcjohnson.synthkit.semgus.reader.user::production
    (&key instance occurrences operator)
  "Creates a production"
  (append (list #|Name:|#operator instance operator) occurrences))

(defun com.kjcjohnson.synthkit.semgus.reader.user::grammar
    (&key non-terminals productions non-terminal-types)
  "Creates a grammar"
  (let ((grammar (g:make-rtg :non-terminals non-terminals
                             :productions productions)))
    (loop for nt-name in non-terminals
          for tt in non-terminal-types
          for nt = (find nt-name
                         (g:non-terminals grammar)
                         :key #'g:name
                         :test #'string=)
          unless nt do (error "Cannot look up NT: ~a" nt) end
          doing
             (setf (g:term-type nt) tt))
    grammar))

(defun com.kjcjohnson.synthkit.semgus.reader.user::synth-fun
    (name &key term-type grammar)
  "Creates a synthesis problem"
  (setf (semgus:grammar semgus:*semgus-context*) grammar)
  (setf (semgus:term-name semgus:*semgus-context*) name)
  (setf (semgus:term-type semgus:*semgus-context*) term-type)
  (let ((possible-root-relations
          (remove-if-not #'(lambda (x) (eql term-type (semgus:term-type x)))
                         (semgus:head-relations semgus:*semgus-context*))))
    (when (endp possible-root-relations)
      (warn "No possible root relations for synth-fun ~a. Make sure CHCs are defined."
            (smt:identifier-string name)))
    (setf (semgus:root-relations semgus:*semgus-context*) possible-root-relations)))

(defun com.kjcjohnson.synthkit.semgus.reader.user::constraint (term)
  "Adds a constraint to the problem"
  (push term (semgus:constraints semgus:*semgus-context*)))

(defun com.kjcjohnson.synthkit.semgus.reader.user::check-synth ()
  "Finishes the synthesis problem definition"
  nil)
