(in-package #:com.kjcjohnson.synthkit.tdp)

(defparameter *g-nt-start* (make-instance 'g:non-terminal :name (string 'start)))
(defparameter *g-nt-b* (make-instance 'g:non-terminal :name (string 'B)))

(defparameter *g-max2-exp*
  (g:make-rtg
   :non-terminals (list *g-nt-start* *g-nt-b*)
   :productions '(
                  ("x" Start x)
                  ("y" Start y)
                  ("0" Start "0")
                  ("1" Start "1")
                  ("+" Start + Start Start)
                  ("ite" Start ite B Start Start)
                  ("true" B true)
                  ("false" B false)
                  ("not" B not B)
                  ("and" B and B B)
                  ("<" B < Start Start))))

(defparameter *g-max2-exp-semantics*
  (lambda (p)
    (let ((n (g:name p)))
      (cond
        ((string= n "x") (list #'(lambda (is) (cdr (assoc 'x is)))))
        ((string= n "y") (list #'(lambda (is) (cdr (assoc 'y is)))))
        ((string= n "0") (list #'(lambda (is) 0)))
        ((string= n "1") (list #'(lambda (is) 1)))
        ((string= n "+") (list #'(lambda (is sem1 sem2)
                             (+ (funcall sem1 is) (funcall sem2 is)))))
        ((string= n "ite") (list #'(lambda (is semb semt seme)
                                     (if (funcall semb is)
                                         (funcall semt is)
                                         (funcall seme is)))))
        ((string= n "true") (list #'(lambda (is) t)))
        ((string= n "false") (list #'(lambda (is) (values nil t))))
        ((string= n "not") (list #'(lambda (is sem1) (values (not (funcall sem1 is)) t))))
        ((string= n "and") (list #'(lambda (is sem1 sem2) (values (and (funcall sem1 is) (funcall sem2 is)) t))))
        ((string= n "<") (list #'(lambda (is sem1 sem2) (values (< (funcall sem1 is) (funcall sem2 is)) t)))))))) 

(defmethod com.kjcjohnson.synthkit.ast::semantics-for-production ((sem (eql *g-max2-exp-semantics*)) p)
  (funcall sem p))

(defparameter *max2-exp* (make-instance 'semgus:semgus-problem
                                        :grammar *g-max2-exp*
                                        :semantics *g-max2-exp-semantics*
                                        :specification (make-instance 'semgus:io-specification)))

(semgus:add-example (semgus:specification *max2-exp*) '((x . 4) (y . 7)) 7)
(semgus:add-example (semgus:specification *max2-exp*) '((x . 9) (y . 2)) 9)
(semgus:add-example (semgus:specification *max2-exp*) '((x . 0) (y . 0)) 0)

(defclass max2-enum-info ()
  ((depth :accessor depth :initarg :depth)
   (specification :reader specification :initarg :specification :initform nil)))

(defparameter *tdp-exp* (make-instance 'tdp-algorithm))

(defparameter *depth-0-specializer* #'(lambda (problem info) (zerop (depth info))))
(defparameter *emptyset-lt* #'(lambda (algo problem nt info) nil))

(defun cart-red (set1 set2)
  (let (output)
    (dolist (x set1)
      (dolist (y set2)
        (push (cons x y) output)))
    output))

(defun cart-all (sets)
  (reduce #'cart-red sets :from-end t :initial-value '(nil)))

(defun enumerator-task (algo problem nt info)
  (let ((program-set nil))
    (dolist (p (g:productions-for-instance (semgus:grammar problem) nt))
      (let* ((k (g:arity p))
             (child-sets (make-sequence 'list k)))
        (loop for i below k
              doing (setf (nth i child-sets)
                          (invoke-learning-task algo
                                                problem
                                                (nth i (g:occurrences p))
                                                (make-instance 'max2-enum-info
                                                               :depth (1- (depth info))))))
        (dolist (ts (cart-all child-sets))
          ;;(when (and (string= (g:name p) "ite") (string= (g:name (a:production (first ts))) "<")) (format t "~A~%" ts))
          (push (make-instance 'a:program-node :production p :children ts) program-set))))
    program-set))

(defun spec-check-specializer (problem info) (not (null (specification info))))
(defun spec-check-learning-task (algo problem nt info)
  (delete-if-not #'(lambda (p)
                     ;;(format t "Checking: ~A~%" p)
                     (every #'(lambda (ex)
                                (semgus:with-example (in out ex)
                                  (equal out (a:execute-program (semgus:semantics problem)
                                                                p
                                                                in))))
                            (semgus:examples
                             (specification info))))
                 (invoke-learning-task algo problem nt (make-instance 'max2-enum-info
                                                                      :depth (depth info)))))

(add-specialized-learning-task *tdp-exp* *g-nt-start* (constantly t) #'enumerator-task)
(add-specialized-learning-task *tdp-exp* *g-nt-b* (constantly t) #'enumerator-task)
          
(add-specialized-learning-task *tdp-exp* *g-nt-start* *depth-0-specializer* *emptyset-lt*)
(add-specialized-learning-task *tdp-exp* *g-nt-b* *depth-0-specializer* *emptyset-lt*)

(add-specialized-learning-task *tdp-exp* *g-nt-start* #'spec-check-specializer #'spec-check-learning-task)
