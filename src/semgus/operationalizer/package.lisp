;;;;
;;;; Operationalizer sub-package
;;;; Exported symbols should be shared with the semgus package.
;;;;
(defpackage #:com.kjcjohnson.synthkit.semgus.operationalizer
  (:use #:cl #:com.kjcjohnson.synthkit.semgus)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:u #:com.kjcjohnson.synthkit.utilities)
                    (#:chc #:com.kjcjohnson.synthkit.semgus.chc)
                    (#:? #:trivia))
  (:export #:operationalize-chc+))

  
