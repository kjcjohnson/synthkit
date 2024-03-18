;;;;
;;;; SemGuS top-level package
;;;;
(defpackage #:com.kjcjohnson.synthkit.semgus
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:u #:com.kjcjohnson.synthkit.utilities)
                    (#:chc #:com.kjcjohnson.synthkit.semgus.chc)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:a #:alexandria)
                    (#:? #:trivia))
  (:export #:semgus-problem
           #:metadata
           #:replace-specification
           #:load-semgus-problem
           #:grammar
           #:semantics
           #:defsemantics
           #:relational-semantics
           #:operational-semantics
           #:specification
           #:context
           #:path
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
           #:symbol-table
           #:constraint)

  ;; Context
  (:export #:lookup-head #:add-head
           #:lookup-root #:lookup-chc #:lookup-chcs-by-operator
           #:semgus-context)

  ;; Reader things
  (:export #:*semgus-context*
           #:*load-semgus-problem-time*
           #:auxiliary-functions
           #:constraints
           #:root-relations
           #:output-names
           #:output-indices
           #:input-names
           #:input-indices
           #:term-type
           #:term-name
           #:term-index)

  ;; Verifier protocol
  (:export #:verify-program
           #:verifier-for-specification
           #:check-program
           #:unknown-verifier-result
           #:*check-program-time*)

  ;; CEGIS
  (:export #:cegis-supported-for-specification?
           #:cegis-supported?
           #:cegis-wrapper
           #:maybe-with-cegis
           #:cegis-next-example)

  ;; Reader protocol
  (:export #:process-chcs-for-relational-semantics
           #:read-problem-from-stream
           #:derive-specification
           #:on-context-load
           #:on-operationalization-load
           #:on-problem-load
           #:register-load-processor)

  ;; Operationalizer protocol
  (:export #:operationalize-chc
           #:*debug-compile* #:when-debug-compile #:if-debug-compile)

  ;; CHC-related exports
  (:export #:arguments #:body #:head #:head-relations
           #:input-variables #:output-variables #:term-name #:variables
           #:input-indexes #:output-indexes #:term-index
            #:output-names #:input-names))
