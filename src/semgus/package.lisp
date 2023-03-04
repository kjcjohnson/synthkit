;;;;
;;;; SemGuS top-level package
;;;;
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

  ;; Reader protocol
  (:export #:process-chcs-for-relational-semantics)

  ;; CHC-related exports
  (:export #:arguments #:body #:head #:head-relations
           #:input-variables #:output-variables #:term-name #:variables
           #:input-indexes #:output-indexes #:term-index
            #:output-names #:input-names))
