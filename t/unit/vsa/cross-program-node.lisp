;;;;
;;;; Empty program node tests
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:in-suite :vsa-tests)

(5am:test cross-program-node/count
  (5am:is (= 4 (vsa:program-count (mk-cpn :arity 2 :count 2))))
  (5am:is (= 1 (vsa:program-count (mk-cpn :arity 3 :count 1))))
  (5am:is (= 8 (vsa:program-count (mk-cpn :arity 3 :count 2)))))

(5am:test cross-program-node/is
  (5am:is-true (vsa:is-cross-program-node? (mk-cpn)))
  (5am:is-false (vsa:is-cross-program-node? (cons 5 6)))
  (5am:is-false (vsa:is-cross-program-node? (make-instance 'vsa:leaf-program-node))))

(5am:test cross-program-node/do-programs
  (let ((iterations 0))
    (vsa:do-programs (p (mk-cpn :arity 3 :count 3))
      (declare (ignore p))
      (incf iterations))
    (5am:is (= 27 iterations)))

  (let ((iterations 0))
    (vsa:do-programs (p (mk-cpn :arity 4 :count 0))
      (declare (ignore p))
      (incf iterations))
    (5am:is (zerop iterations)))

  (let ((iterations 0))
    (vsa:do-programs (p (mk-cpn :arity 3 :count 1))
      (declare (ignore p))
      (incf iterations))
    (5am:is (= 1 iterations))))
