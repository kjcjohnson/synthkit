;;;;
;;;; Grammar package
;;;;
(defpackage #:com.kjcjohnson.synthkit.grammar
  (:use #:cl)
  (:local-nicknames (#:u #:com.kjcjohnson.synthkit.utilities))
  (:export #:regular-tree-grammar
           #:initial-non-terminal
           #:productions-for-instance
           #:term-type
           #:non-terminal
           #:non-terminals
           #:operator
           #:arity
           #:production
           #:productions
           #:instance
           #:occurrences
           #:name
           #:make-rtg
           #:extra-data

           #:distance-to-leaves #:compute-distance-to-leaves
           )
  ;; Searches
  (:export #:lookup-non-terminal #:lookup-operator #:lookup-production
           #:non-terminals-for-term-type))
