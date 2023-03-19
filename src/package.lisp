;;;
;;; Package definitions
;;;
(defpackage #:com.kjcjohnson.synthkit.utilities
  (:use #:cl)
  (:local-nicknames (#:? #:trivia))
  (:export #:choose-uniformly-at-random
           #:copy-instance
           #:ensure-vector
           #:ensure-list
           #:stash))
