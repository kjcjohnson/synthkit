;;;;
;;;; Theory definitions for bit vectors
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defun clip (bv bv-template)
  "Clips BV to same size as BV-TEMPLATE"
  (declare (type bit-vector bv bv-template))
  (let* ((size (length bv-template))
         (new (make-array size :element-type 'bit)))

    (loop for ix from 0 below size
          for src = (+ ix (- (length bv) size))
          doing (setf (bit new ix)
                      (if (< src 0)
                          0
                          (bit bv src))))
    new))


(defsmtfun "bvnot" :bv (bv)
  "Bit vector not"
  (declare (type bit-vector bv))
  (bit-not bv))

(defsmtfun "bvand" :bv (bv1 bv2)
  "Bit vector and"
  (declare (type bit-vector bv1 bv2))
  (bit-and bv1 bv2))

(defsmtfun "bvor" :bv (bv1 bv2)
  "Bit vector inclusive or"
  (declare (type bit-vector bv1 bv2))
  (bit-ior bv1 bv2))

(defsmtfun "bvxor" :bv (bv1 bv2)
  "Bit vector exclusive or"
  (declare (type bit-vector bv1 bv2))
  (bit-xor bv1 bv2))

(defsmtfun "bvlshr" :bv (bv count)
  "Bit vector shift right"
  (declare (type bit-vector bv count))
  (clip
   (bit-smasher:rshift bv (bit-smasher:bits->int count))
   bv))

(defsmtfun "bvshl" :bv (bv count)
  "Bit vector shift left"
  (declare (type bit-vector bv count))
  (clip
   (bit-smasher:lshift bv (bit-smasher:bits->int count))
   bv))

(defsmtfun "bvadd" :bv (bv1 bv2)
  "Bit vector addition (modulo length)"
  (declare (type bit-vector bv1 bv2))
  (clip
   (bit-smasher:bit-sum bv1 bv2)
   bv1))

(defsmtfun "bvsub" :bv (bv1 bv2)
  "Bit vector subtraction (modulo length)"
  (declare (type bit-vector bv1 bv2))
  (clip
   (bit-smasher:bit-sum bv1 (bit-not bv2) 1)
   bv1))

(defsmtfun "bvneg" :bv (bv)
  "Bit vector 2's complement negation"
  (declare (type bit-vector bv))
  (clip
   (bit-smasher:bit-sum (bit-not bv) 1)
   bv))

(defsmtfun "bvult" :bv (bv1 bv2)
  "Bit vector unsigned less-than"
  (declare (type bit-vector bv1 bv2))
  (< (bit-smasher:int<- bv1) (bit-smasher:int<- bv2)))

(defsmtfun ("extract" i j) :bv (bv)
  "Extracts bits I down to J from BV"
  (declare (type bit-vector bv))
  (declare (type integer i j))
  (assert (and (> (length bv) i)
               (>= i j 0)))
  ;; Extract bits starting from J through I
  ;; But bitvectors in CL are backwards
  (let ((start (- (length bv) i 1))
        (end (- (length bv) j)))
    (subseq bv start end)))
