;;;;
;;;; Leaf program node tests
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:in-suite :vsa-tests)

(5am:test leaf-program-node/count
  (5am:is (= 1 (vsa:program-count (mk-lpn)))))

(5am:test leaf-program-node/is
  (5am:is-true (vsa:is-leaf-program-node? (mk-lpn)))
  (5am:is-false (vsa:is-leaf-program-node? (cons 5 6)))
  (5am:is-false (vsa:is-leaf-program-node? (make-instance 'vsa:empty-program-node))))

(5am:test leaf-program-node/do-programs
  (let ((iterations 0))
    (vsa:do-programs (p (mk-lpn))
      (declare (ignore p))
      (incf iterations))
    (5am:is (= 1 iterations))))
