;;;;
;;;; The verifiers package
;;;;
(defpackage #:com.kjcjohnson.synthkit.semgus.verifiers
  (:use #:cl)
  (:local-nicknames (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:chc #:com.kjcjohnson.synthkit.semgus.chc)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:u #:com.kjcjohnson.synthkit.utilities)
                    (#:a #:alexandria)
                    (#:* #:serapeum/bundle)))
