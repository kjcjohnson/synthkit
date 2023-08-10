;;;;
;;;; Package for solver implementations
;;;;
(defpackage #:com.kjcjohnson.synthkit.smt.solvers
  (:use #:cl)
  (:import-from #:cl-smt-lib/process-two-way-stream #:process-two-way-stream)
  (:local-nicknames (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:a #:alexandria)
                    (#:? #:trivia)))
