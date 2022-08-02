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
    

(defun bv-not (bv)
  "Bit vector not"
  (declare (type bit-vector bv))
  (bit-not bv))

(defun bv-and (bv1 bv2)
  "Bit vector and"
  (declare (type bit-vector bv1 bv2))
  (bit-and bv1 bv2))

(defun bv-or (bv1 bv2)
  "Bit vector inclusive or"
  (declare (type bit-vector bv1 bv2))
  (bit-ior bv1 bv2))

(defun bv-xor (bv1 bv2)
  "Bit vector exclusive or"
  (declare (type bit-vector bv1 bv2))
  (bit-xor bv1 bv2))

(defun bv-lshr (bv count)
  "Bit vector shift right"
  (declare (type bit-vector bv count))
  (clip
   (bit-smasher:rshift bv (bit-smasher:bits->int count))
   bv))

(defun bv-shl (bv count)
  "Bit vector shift left"
  (declare (type bit-vector bv count))
  (clip
   (bit-smasher:lshift bv (bit-smasher:bits->int count))
   bv))

(defun bv-add (bv1 bv2)
  "Bit vector addition (modulo length)"
  (declare (type bit-vector bv1 bv2))
  (clip
   (bit-smasher:bit-sum bv1 bv2)
   bv1))
