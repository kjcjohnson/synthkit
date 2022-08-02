;;;
;;; Package definitions
;;;
(defpackage #:com.kjcjohnson.synthkit.utilities
  (:use #:cl)
  (:export #:choose-uniformly-at-random
           #:copy-instance))

(defpackage #:com.kjcjohnson.synthkit.smt
  (:use #:cl)
  (:shadow #:sort #:variable)
  (:export #:name #:children #:to-smt #:definition #:sort #:child-sorts #:arity
           #:*int-sort* #:*bool-sort* #:*string-sort* #:variable
           #:$int #:$bool #:$string #:$function
           #:$exists #:$forall #:$true #:$false #:$apply
           #:$+ #:$- #:$< #:$> #:$* #:$=
           #:$not #:$and #:$or #:$xor
           #:$iff #:$implies #:$ite
           #:with-solver #:make-solver #:close-solver #:check-sat #:get-model
           #:push-scope #:pop-scope #:with-scope
           #:add #:declare-constants #:dump-commands #:set-model

           #:*smt* #:init-smt
           #:ensure-identifier #:ensure-sort #:identifier-string

           #:get-function-definition #:set-function-definition
           #:get-compiled-function

           #:is-application?))

(defpackage #:com.kjcjohnson.synthkit.grammar
  (:use #:cl)
  (:local-nicknames (#:u #:com.kjcjohnson.synthkit.utilities))
  (:export #:regular-tree-grammar
           #:initial-non-terminal
           #:productions-for-instance
           #:non-terminal
           #:non-terminals
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
           #:operational-semantics-for-production
           #:relational-semantics-for-production
           #:relational-semantics-for-non-terminal
           #:children
           #:nth-child
           #:swap-nth-child
           #:program-size
           #:copy-program
           #:compile-program
           #:execute-program
           #:as-smt-query))

(defpackage #:com.kjcjohnson.synthkit.vsa
  (:use #:cl)
  (:local-nicknames (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:kl #:com.kjcjohnson.kale)
                    (#:kl/c #:com.kjcjohnson.kale.collections)
                    (#:kl/oo #:com.kjcjohnson.kale.oo))
  (:export #:filter
           #:prune))

(defpackage #:com.kjcjohnson.synthkit.semgus
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:u #:com.kjcjohnson.synthkit.utilities))
  (:export #:semgus-problem
           #:load-semgus-problem
           #:grammar
           #:semantics
           #:defsemantics
           #:relational-semantics
           #:operational-semantics
           #:specification
           #:context
           #:chcs
           #:name
           #:constructor
           #:io-specification
           #:examples
           #:add-example
           #:example-input
           #:example-output
           #:with-example
           #:do-examples
           #:formula-specification
           #:formula
           #:relation-name
           #:cegis-specification
           #:find-counter-example-for-specification
           #:find-counter-example
           #:counter-examples
           #:add-counter-example
           #:add-counter-example-for-specification
           #:ensure-cegis-problem
           #:constraint
           ))

(defpackage #:com.kjcjohnson.synthkit.tdp
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:a #:com.kjcjohnson.synthkit.ast)
                    (#:semgus #:com.kjcjohnson.synthkit.semgus))) 
