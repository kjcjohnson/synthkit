;;;;
;;;; Empty program node tests
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:in-suite :vsa-tests)

(5am:test empty-program-node/count
  (5am:is (= 0 (vsa:program-count (mk-epn)))))

(5am:test empty-program-node/is
  (5am:is-true (vsa:is-empty-program-node? (mk-epn)))
  (5am:is-false (vsa:is-empty-program-node? (cons 5 6)))
  (5am:is-false (vsa:is-empty-program-node? (make-instance 'vsa:leaf-program-node))))

(5am:test empty-program-node/do-programs
  (let ((iterations 0))
    (vsa:do-programs (p (mk-epn))
      (declare (ignore p))
      (incf iterations))
    (5am:is (zerop iterations))))
