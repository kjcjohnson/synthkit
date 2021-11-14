;;;
;;; Package definitions
;;;
(defpackage #:com.kjcjohnson.synthkit.utilities
  (:use #:cl)
  (:export #:choose-uniformly-at-random))

(defpackage #:com.kjcjohnson.synthkit.smt
  (:use #:cl)
  (:shadow #:sort #:variable
           #:+ #:- #:< #:> #:* #:=
           #:not #:and #:or #:xor
           #:add
           )
  (:export #:name #:children #:to-smt #:definition #:sort #:child-sorts #:arity
           #:*int-sort* #:*bool-sort* #:*string-sort*
           #:$int #:$bool #:$string #:$function #:$exists #:$forall #:$true #:$false #:$apply
           #:+ #:- #:< #:> #:* #:= #:not #:and #:or #:xor #:iff #:implies #:ite))

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
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:smt #:com.kjcjohnson.synthkit.smt))
  (:export #:program-node
           #:production
           #:operator
           #:print-program-operator
           #:semantics-for-production
           #:relational-semantics-for-production
           #:relational-semantics-for-non-terminal
           #:children
           #:nth-child
           #:swap-nth-child
           #:program-size
           #:copy-program
           #:compile-program
           #:execute-program))

(defpackage #:com.kjcjohnson.synthkit.semgus
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:a #:com.kjcjohnson.synthkit.ast)
                    (#:smt #:com.kjcjohnson.synthkit.smt))
  (:export #:semgus-problem
           #:grammar
           #:semantics
           #:specification
           #:io-specification
           #:examples
           #:add-example
           #:example-input
           #:example-output
           #:with-example
           #:do-examples
           #:formula-specification
           #:formula
           #:relation-name))

(defpackage #:com.kjcjohnson.synthkit.tdp
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:a #:com.kjcjohnson.synthkit.ast)
                    (#:semgus #:com.kjcjohnson.synthkit.semgus))) 
