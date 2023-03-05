;;;;
;;;; Version-space algebra package
;;;;
(defpackage #:com.kjcjohnson.synthkit.vsa
  (:use #:cl)
  (:local-nicknames (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:kl #:com.kjcjohnson.kale)
                    (#:kl/c #:com.kjcjohnson.kale.collections)
                    (#:kl/oo #:com.kjcjohnson.kale.oo))
  (:export #:filter
           #:prune))
