;;;;
;;;; Theory tests
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:def-suite :smt-theories :in :smt-tests)
(5am:in-suite :smt-theories)

;;;
;;; Type tests
;;;
(5am:test smt-index-type
  (5am:is-true (typep "test" 'smt::smt-index-type))
  (5am:is-true (typep 1 'smt::smt-index-type))
  (5am:is-false (typep 'x 'smt::smt-index-type))
  (5am:is-true (typep 'x '(smt::smt-index-type :concrete nil)))
  (5am:is-false (typep 3.3 'smt::smt-index-type)))

(5am:test smt-simple-name-type
  (5am:is-true (typep "test" 'smt::smt-simple-name-type))
  (5am:is-false (typep '("test" 1 2) 'smt::smt-simple-name-type))
  (5am:is-false (typep 5 'smt::smt-simple-name-type)))

(5am:test smt-indexed-name-type
  (5am:is-false (typep "test" 'smt::smt-indexed-name-type))
  (5am:is-true (typep '("test" 1 2) 'smt::smt-indexed-name-type))
  (5am:is-false (typep '("test" 1 2 q) 'smt::smt-indexed-name-type))
  (5am:is-true (typep '("test" 1 2 q) '(smt::smt-indexed-name-type :concrete nil)))
  (5am:is-false (typep '(x 1 2) 'smt::smt-indexed-name-type)))

(5am:test smt-name-type
  (5am:is-true (typep "test" 'smt::smt-name-type))
  (5am:is-true (typep '("test" 1 "2") 'smt::smt-name-type))
  (5am:is-false (typep '("test" 1 2 q) 'smt::smt-name-type))
  (5am:is-true (typep '("test" 1 2 q) '(smt::smt-name-type :concrete nil)))
  (5am:is-false (typep 'x 'smt::smt-name-type)))

;;;
;;; Dispatch tests
;;;
(5am:test indexed-name-parsing
  (5am:is (equal '("test" t nil nil)
                 (multiple-value-list (smt::%parse-indexed-name "test"))))
  (5am:is (equal '("test" t nil nil)
                 (multiple-value-list (smt::%parse-indexed-name '("test")))))
  (5am:is (equal '("(test 1 x)" t nil nil)
                 (multiple-value-list (smt::%parse-indexed-name '("test" 1 "x")))))
  (5am:is (equal '("test;2" nil (0 1) (i j))
                 (multiple-value-list (smt::%parse-indexed-name '("test" i j)))))
  (5am:is (equal '("test;3" nil (2) (q))
                 (multiple-value-list (smt::%parse-indexed-name '("test" "x" 5 q))))))
