;;;;
;;;; Function ranks
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defclass rank ()
  ((argument-sorts :reader argument-sorts
                   :initarg :argument-sorts
                   :type vector
                   :documentation "Function argument sorts")
   (return-sort :reader return-sort
                :initarg :return-sort
                :type sort
                :documentation "Return sort of this function")))

(defun make-rank (argument-sorts return-sort)
  "Creates a rank"
  (make-instance 'rank :argument-sorts (u:ensure-vector argument-sorts)
                       :return-sort return-sort))
