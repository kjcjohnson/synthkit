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

           #:semantics-descriptor-requests
           #:semantic-builder-function
           #:semantics-descriptor-request-descriptor
           #:semantics-descriptor-request-node-id
           #:calling-card
           #:semantics-descriptor-request

           #:production
           #:operator
           #:print-program-operator
           #:semantics-descriptors-for-non-terminal
           #:operational-semantics-for-production
           #:relational-semantics-for-production
           #:relational-semantics-for-non-terminal
           #:children
           #:nth-child
           #:swap-nth-child
           #:program-size
           #:has-hole?
           #:copy-program
           #:compile-program
           #:execute-program
           #:as-smt-query
           #:*execution-counter*))
