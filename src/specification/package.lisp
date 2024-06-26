;;;;
;;;; Specification types
;;;;
(defpackage #:com.kjcjohnson.synthkit.specification
  (:use #:cl)
  (:local-nicknames (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:a #:alexandria))
  ;; Specification types
  (:export #:specification
           #:relational-specification
           #:universal-specification
           #:existential-specification
           #:cegis-specification
           #:compound-specification
           #:intersection-specification
           #:union-specification
           #:inductive-specification
           #:io-specification)
  ;; Accessors
  (:export #:expression
           #:descriptor
           #:descriptors
           #:components
           #:input-state
           #:output-state
           #:input-symbols
           #:output-symbols
           #:input-sorts
           #:output-sorts
           #:relation
           #:relational-specification
           #:constraint
           #:cegis-examples
           #:predicate)

  ;; Methods
  (:export #:leaf-specification-types
           #:compound-specification-types

           #:is-only?
           #:is-only-inductive?
           #:is-only-io?
           #:with-only?
           #:with-only-intersection?
           #:with-only-union?

           #:is-pbe? #:is-universal? #:is-existential? #:is-relational?

           #:examples
           #:leaves
           #:filter-examples
           #:filter-examples-by-descriptor

           #:is-cegis?
           #:convert-to-cegis
           #:add-example
           #:clear-examples
           #:cegis-supported-for-specification?))
