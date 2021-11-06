;;;
;;; Package definitions
;;;
(defpackage #:com.kjcjohnson.synthkit.utilities
  (:use #:cl)
  (:export #:choose-uniformly-at-random))

(defpackage #:com.kjcjohnson.synthkit.grammar
  (:use #:cl)
  (:local-nicknames (#:u #:com.kjcjohnson.synthkit.utilities))
  (:export #:regular-tree-grammar
           #:initial-non-terminal
           #:productions-for-instance
           #:non-terminal
           #:operator
           #:arity
           #:production
           #:instance
           #:occurrences
           #:name
           #:make-rtg
           ))

(defpackage #:com.kjcjohnson.synthkit.ast
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar))
  (:export #:program-node
           #:production
           #:operator
           #:semantics-for-production
           #:children
           #:nth-child
           #:swap-nth-child
           #:program-size
           #:compile-program
           #:execute-program))

(defpackage #:com.kjcjohnson.synthkit.semgus
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:a #:com.kjcjohnson.synthkit.ast)))
