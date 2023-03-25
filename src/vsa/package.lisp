;;;;
;;;; Version-space algebra package
;;;;
(defpackage #:com.kjcjohnson.synthkit.vsa
  (:use #:cl)
  (:local-nicknames (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:g #:com.kjcjohnson.synthkit.grammar))

  ;; Utility operations
  (:export #:filter
           #:prune)

  ;; Program node types and predicates
  (:export #:program-node       #:is-program-node?
           #:empty-program-node #:make-empty-program-node #:is-empty-program-node?
           #:leaf-program-node  #:make-leaf-program-node  #:is-leaf-program-node?
           #:union-program-node #:is-union-program-node?
           #:cross-program-node #:is-cross-program-node?)

  ;; Program node protocol
  (:export #:program-count
           #:scan)

  ;; Enumeration
  (:export #:enumerator #:enumerate-programs #:do-programs))
