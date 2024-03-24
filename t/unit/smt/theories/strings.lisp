;;;;
;;;; String tests
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:def-suite :strings :in :smt-theories)
(5am:in-suite :strings)

(5am:test str.indexof/not-found
  (5am:is (= -1 (smt:call-smt "str.indexof" "abc" "." 0)))
  (5am:is (= -1 (smt:call-smt "str.indexof" "abcd" "a" 2))))

(5am:test str.indexof/simple
  (5am:is (= 0 (smt:call-smt "str.indexof" "abc" "abc" 0)))
  (5am:is (= 1 (smt:call-smt "str.indexof" "xabc" "abc" 0)))
  (5am:is (= 2 (smt:call-smt "str.indexof" "xyzxyz" "z" 0))))

(5am:test str.indexof/start
  (5am:is (= -1 (smt:call-smt "str.indexof" "abc" "abc" 1)))
  (5am:is (= 1 (smt:call-smt "str.indexof" "xabc" "abc" 1)))
  (5am:is (= 3 (smt:call-smt "str.indexof" "xabab" "ab" 2)))
  (5am:is (= 2 (smt:call-smt "str.indexof" "xyzxyz" "z" 2)))
  (5am:is (= 5 (smt:call-smt "str.indexof" "xyzxyz" "z" 3))))

(5am:test str.indexof/empty
  (5am:is (= 0 (smt:call-smt "str.indexof" "abc" "" 0)))
  (5am:is (= 2 (smt:call-smt "str.indexof" "abc" "" 2)))
  (5am:is (= -1 (smt:call-smt "str.indexof" "abc" "" 5)))
  (5am:is (= -1 (smt:call-smt "str.indexof" "abc" "" -7)))
  (5am:is (= 0 (smt:call-smt "str.indexof" "" "" 0))))
