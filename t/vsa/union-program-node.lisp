;;;;
;;;; Empty program node tests
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:in-suite :vsa-tests)

(5am:test union-program-node/count
  (5am:is (= 5 (vsa:program-count (mk-upn :count 5)))))

(5am:test union-program-node/is
  (5am:is-true (vsa:is-union-program-node? (mk-upn)))
  (5am:is-false (vsa:is-union-program-node? (cons 5 6)))
  (5am:is-false (vsa:is-union-program-node? (make-instance 'vsa:leaf-program-node))))

(5am:test union-program-node/do-programs
  (let ((iterations 0))
    (vsa:do-programs (p (mk-upn :count 5))
      (declare (ignore p))
      (incf iterations))
    (5am:is (= 5 iterations))))
