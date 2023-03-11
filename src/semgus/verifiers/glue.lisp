;;;;
;;;; Glue verifiers
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.verifiers)

(defclass intersection-verifier ()
  ()
  (:documentation "Verifies that each sub-specification holds"))
(defparameter *intersection-verifier-instance* (make-instance 'intersection-verifier))

(defclass union-verifier ()
  ()
  (:documentation "Verifies that at least one sub-specification holds"))
(defparameter *union-verifier-instance* (make-instance 'union-verifier))

(defmethod semgus:verifier-for-specification
    ((spec spec:intersection-specification) semgus-problem &key produce-cex)
  (declare (ignore spec semgus-problem produce-cex))
  *intersection-verifier-instance*)

(defmethod semgus:verifier-for-specification
    ((spec spec:union-specification) semgus-problem &key produce-cex)
  (declare (ignore spec semgus-problem produce-cex))
  *union-verifier-instance*)

(defmethod semgus:verify-program
    ((verifier intersection-verifier) specification semgus-problem program
     &key produce-cex)
  (assert (typep specification 'spec:intersection-specification))
  (loop for child-spec in (spec:components specification)
        for child-verifier = (semgus:verifier-for-specification
                              child-spec semgus-problem :produce-cex produce-cex)
        do (multiple-value-bind (result cex)
               (semgus:verify-program child-verifier child-spec semgus-problem program
                                      :produce-cex produce-cex)
             (case result
               (:invalid (return-from semgus:verify-program (values result cex)))
               (:unknown (return-from semgus:verify-program result)))))
  :valid)

(defmethod semgus:verify-program
    ((verifier union-verifier) specification semgus-problem program
     &key produce-cex)
  (assert (typep specification 'spec:union-specification))
  (let ((first-cex nil))
    (loop for child-spec in (spec:components specification)
          for child-verifier = (semgus:verifier-for-specification
                                child-spec semgus-problem :produce-cex produce-cex)
          do (multiple-value-bind (result cex)
                 (semgus:verify-program child-verifier child-spec semgus-problem program
                                        :produce-cex produce-cex)
               (case result
                 (:valid (return-from semgus:verify-program result))
                 (:unknown (return-from semgus:verify-program result)))

               (when (and cex (null first-cex))
                 (setf first-cex cex))))
    (values :invalid first-cex)))
