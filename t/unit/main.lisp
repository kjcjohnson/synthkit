;;;;
;;;; Main test suite entry
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:def-suite :synthkit-tests)
(5am:in-suite :synthkit-tests)

(5am:def-test fiveam-test ()
  (5am:is-true t))
