;;;;
;;;; Test package
;;;;
(defpackage #:com.kjcjohnson.synthkit/test
  (:use #:cl)
  (:local-nicknames (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:vsa #:com.kjcjohnson.synthkit.vsa))
  (:export #:synthkit-tests))
