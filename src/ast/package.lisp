;;;;
;;;; AST package
;;;;
(defpackage #:com.kjcjohnson.synthkit.ast
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:a #:alexandria))
  (:export #:program-atom
           #:program-node
           #:program-hole
           #:print-program
           #:break-on-program

           #:semantics-descriptor-requests
           #:semantic-builder-function
           #:semantics-descriptor-request-descriptor
           #:semantics-descriptor-request-node-id
           #:calling-card
           #:semantics-descriptor-request

           #:production
           #:operator
           #:non-terminal
           #:print-program-operator
           #:print-program-node-as-smt
           #:semantics-descriptors-for-non-terminal
           #:operational-semantics-for-production
           #:operational-semantics-for-hole
           #:relational-semantics-for-production
           #:relational-semantics-for-non-terminal
           #:children
           #:nth-child
           #:swap-nth-child
           #:program-size
           #:has-hole?
           #:hole-count
           #:copy-program
           #:compile-program
           #:execute-program
           #:as-smt-query)
  ;; Debugging controls - EXPORTED BUT UNSTABLE
  (:export #:*exe-debug*
           #:*exe-level*
           #:*exe-debug-match*)
  ;; Ugh. I'd rather not export these as variables
  (:export #:*root-input-state*
           #:*root-input-descriptor*)
  ;; Counters
  (:export #:*execution-counter*
           #:*checkpoint-times*
           #:*program-trace-stream*
           #:add-checkpoint
           #:clear-all-checkpoints
           #:trace-program
           #:*candidate-concrete-programs*
           #:*candidate-partial-programs*
           #:*concrete-candidates-by-size*
           #:*prune-candidate-counter*
           #:*prune-attempt-counter*
           #:*prune-success-counter*))
