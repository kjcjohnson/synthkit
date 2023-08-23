;;;;
;;;; Testing conversions to smt
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:def-suite* :to-smt-tests :in :smt-tests)

(5am:test to-smt/literal/bitvector
  (let ((symbolic (smt:to-smt #*1010 :pprint nil))
        (pretty (smt:to-smt #*1010 :pprint t)))
    ;; Non-pretty symbolic is a symbol with name of bitvec representation (yuck!)
    (5am:is (typep symbolic 'smt::smt-bv-wrapper))
    (5am:is (string= "#b1010" (princ-to-string symbolic)))
    ;; Pretty is just a string
    (5am:is (stringp pretty))
    (5am:is (string= "#b1010" pretty))))
