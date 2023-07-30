;;;;
;;;; CHCs package
;;;;
(defpackage #:com.kjcjohnson.synthkit.semgus.chc
  (:use #:cl)
  (:local-nicknames (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:a #:alexandria))
  (:shadow #:symbol-name)
  ;; Relations and constructors
  (:export #:constructor
           #:name
           #:arguments
           #:argument-sorts
           #:return-sort

           #:head
           #:forward-declared-head
           #:signature
           #:roles #:formals
           #:is-forward-declared-head? #:fixup-forward-declared-head
           #:make-head-from-relation
           #:term-index #:term-type #:term-name
           #:filter-role
           #:role-indices
           #:input-indices #:input-formals #:output-indices #:output-formals
           #:output-actuals
           #:relation #:head #:actuals)
  ;; CHCs
  (:export #:chc
           #:symbol-table #:head #:body #:constraint #:constructor #:data #:id)
  ;; Symbol tables
  (:export #:symbol-entry
           #:symbol-name #:symbol-sort #:symbol-index

           #:symbol-table
           #:input-symbols #:output-symbols
           #:term-symbol
           #:auxiliary-symbols #:child-symbols)
  ;; Other operations
  (:export #:body-to-smt #:production-for-chc))
