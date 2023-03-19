;;;;
;;;; VSA tests
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:def-suite :vsa-tests :in :synthkit-tests)
(5am:in-suite :vsa-tests)

;;;
;;; Mocks
;;;
(defun mk-opr (arity)
  "Makes a mock operator"
  (make-instance 'g:operator
                 :name "test"
                 :arity arity))

(defun mk-prd (arity)
  "Makes a mock production"
  (let ((nt (make-instance 'g:non-terminal :name "testNT")))
    (make-instance 'g:production
                   :name "test"
                   :occurrences (make-list arity :initial-element nt)
                   :instance nt
                   :operator (mk-opr arity))))

(defun mk-epn ()
  (make-instance 'vsa:empty-program-node))

(defun mk-lpn ()
  (make-instance 'vsa:leaf-program-node
                 :program (make-instance 'ast:program-node
                                         :operator (mk-opr 0))))

(defun mk-upn (&key (count 5) (contents nil contents-supplied?))
  (unless contents-supplied?
    (setf contents (make-list count :initial-element (mk-lpn))))
  (make-instance 'vsa:union-program-node
                 :programs contents))

(defun mk-cpn (&key (count 2) (arity 2) production)
  (unless production
    (setf production (mk-prd arity)))
  (let* ((set (mk-upn :contents (make-list count :initial-element (mk-lpn))))
         (sets (make-list arity :initial-element set)))
    (make-instance 'vsa:cross-program-node
                   :sets sets
                   :production production)))
