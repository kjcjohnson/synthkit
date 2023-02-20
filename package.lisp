;;;
;;; Package definitions
;;;
(defpackage #:com.kjcjohnson.synthkit.utilities
  (:use #:cl)
  (:export #:choose-uniformly-at-random
           #:copy-instance))

(defpackage #:com.kjcjohnson.synthkit.smt
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
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
           #:get-compiled-function #:evaluate-expression

           #:get-sort
           #:datatype #:datatype-constructor #:constructors
           #:add-datatype-constructor
           #:datatype=

           #:state #:make-state #:copy-state #:get-value #:state=
           #:evaluate-state #:make-temp-state #:canonicalize-state
           #:get-first-value

           #:get-constant-type

           #:is-application?

           #:do-call-smt #:call-smt))

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
           ))

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
                    (#:u #:com.kjcjohnson.synthkit.utilities)
                    (#:a #:alexandria)
                    (#:? #:trivia))
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
           #:example-descriptor
           #:examples-count
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
           #:constraint)

  ;; Reader things
  (:export #:*semgus-context*
           #:constraints
           #:root-relations
           #:output-names
           #:output-indices
           #:input-names
           #:input-indices
           #:term-name
           #:term-index)
  
  ;; Verifier protocol
  (:export #:verify-program)
           
  ;; CHC-related exports
  (:export #:arguments #:body #:head #:head-relations
           #:input-variables #:output-variables #:term-name #:variables
           #:input-indexes #:output-indexes #:term-index
            #:output-names #:input-names))

(defpackage #:com.kjcjohnson.synthkit.semgus.reader
  (:use #:cl)
  (:local-nicknames (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:smt #:com.kjcjohnson.synthkit.smt)))

(defpackage #:com.kjcjohnson.synthkit.semgus.verifiers
  (:use #:cl)
  (:local-nicknames (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:smt #:com.kjcjohnson.synthkit.smt)))

(defpackage #:com.kjcjohnson.synthkit.tdp
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:a #:com.kjcjohnson.synthkit.ast)
                    (#:semgus #:com.kjcjohnson.synthkit.semgus)))
