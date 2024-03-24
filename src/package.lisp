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
           #:set-slot-if-unbound
           #:stash)

  ;; Timing utilities
  (:export #:with-timing
           #:declare-timed-section
           #:with-timed-section
           #:get-timed-section-real-time
           #:get-timed-section-gc-time
           #:get-timed-section-bytes-consed
           #:reset-timed-section-time))
