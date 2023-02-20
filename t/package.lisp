;;;;
;;;; Test package
;;;;
(defpackage #:com.kjcjohnson.synthkit/test
  (:use #:cl)
  (:local-nicknames (#:smt #:com.kjcjohnson.synthkit.smt))
  (:export #:synthkit-tests))
