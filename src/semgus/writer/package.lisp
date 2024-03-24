;;;;
;;;; The writer package - for writing and serializing SemGuS data
;;;;
(defpackage #:com.kjcjohnson.synthkit.semgus.writer
  (:use #:cl)
  (:local-nicknames (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:chc #:com.kjcjohnson.synthkit.semgus.chc)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:u #:com.kjcjohnson.synthkit.utilities)
                    (#:? #:trivia)
                    (#:* #:serapeum/bundle)
                    (#:jzon #:com.inuoe.jzon)
                    (#:mop #:closer-mop)))
