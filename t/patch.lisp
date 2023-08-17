;;;;
;;;; Quick! Monkey-patch 5am while no one is looking!
;;;;
;;;; Basically, nested suites have their names duplicated in some circumstances.
;;;;
(in-package #:it.bese.fiveam)

(defun (setf get-test) (value key)
  (pushnew key (%test-names *test*) :test 'eql)
  (setf (gethash key (%tests *test*)) value))
