;;;;
;;;; String tests
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:def-suite :bitvectors :in :smt-theories)
(5am:in-suite :bitvectors)

(5am:test bvnot/works
  (5am:is (equal #*1111 (smt:call-smt "bvnot" #*0000)))
  (5am:is (equal #*0101 (smt:call-smt "bvnot" #*1010))))

(5am:test bvand/works
  (5am:is (equal #*101 (smt:call-smt "bvand" #*101 #*111)))
  (5am:is (equal #*00100 (smt:call-smt "bvand" #*10101 #*00110))))

(5am:test bvor/works
  (5am:is (equal #*101 (smt:call-smt "bvor" #*101 #*100)))
  (5am:is (equal #*10111 (smt:call-smt "bvor" #*10101 #*00110))))

(5am:test bvxor/works
  (5am:is (equal #*001 (smt:call-smt "bvxor" #*101 #*100)))
  (5am:is (equal #*10011 (smt:call-smt "bvxor" #*10101 #*00110))))

(5am:test bvlshr/works
  (5am:is (equal #*010 (smt:call-smt "bvlshr" #*101 #*001)))
  (5am:is (equal #*00111 (smt:call-smt "bvlshr" #*11101 #*00010))))

(5am:test bvshl/works
  (5am:is (equal #*100 (smt:call-smt "bvshl" #*101 #*010)))
  (5am:is (equal #*01000 (smt:call-smt "bvshl" #*11101 #*00011))))

(5am:test bvadd/works
  (5am:is (equal #*111 (smt:call-smt "bvadd" #*101 #*010)))
  (5am:is (equal #*00010 (smt:call-smt "bvadd" #*11101 #*00101))))

(5am:test bvsub/works
  (5am:is (equal #*000 (smt:call-smt "bvsub" #*101 #*101)))
  (5am:is (equal #*11000 (smt:call-smt "bvsub" #*11101 #*00101)))
  (5am:is (equal #*1111 (smt:call-smt "bvsub" #*0001 #*0010)))
  (5am:is (equal #*1010 (smt:call-smt "bvsub" #*0000 #*0110))))

(5am:test bvneg/works
  (5am:is (equal #*1111 (smt:call-smt "bvneg" #*0001)))
  (5am:is (equal #*00 (smt:call-smt "bvneg" #*00)))
  (5am:is (equal #*010 (smt:call-smt "bvneg" #*110))))

(5am:test bvult/works
  (5am:is-true (smt:call-smt "bvult" #*0001 #*0010))
  (5am:is-true (smt:call-smt "bvult" #*0001 #*1111))
  (5am:is-false (smt:call-smt "bvult" #*0001 #*0000))
  (5am:is-false (smt:call-smt "bvult" #*0011 #*0011)))

(5am:test extract/works
  (5am:is (equal #*0 (smt:call-smt ("extract" 0 0) #*10)))
  (5am:is (equal #*1 (smt:call-smt ("extract" 1 1) #*10)))
  (5am:is (equal #*10 (smt:call-smt ("extract" 1 0) #*10)))
  (5am:is (equal #*11 (smt:call-smt ("extract" 2 1) #*0110))))

(5am:test concat/works
  (5am:is (equal #*0011 (smt:call-smt "concat" #*00 #*11)))
  (5am:is (equal #*11011 (smt:call-smt "concat" #*110 #*11))))
