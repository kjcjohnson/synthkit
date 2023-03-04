;;;;
;;;; A simple concretizing verifier - can handle loop-free programs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.verifiers)

(defclass concretizing-verifier ())

(defun query-smt (produce-cex)
  (smt:with-solver (solver smt::*cvc4*)
    (smt:dump-commands solver
                        (ast::as-smt-query program
                                           (semantics problem)
                                           (relation-name spec)))
    
    (smt:declare-constants solver (formula spec))
    (smt:add solver (smt:$not (formula spec)))
    
    (let ((q-res (smt:check-sat solver)))
      (cond
        ((eql :unknown q-res)
         (values :unknown nil))
        ((eql :unsat q-res)
         (values :invalid nil))
        ((eql :sat q-res)
         (values :valid (when produce-cex (smt:get-model solver))))
        (t (error "Invalid SMT response: ~s" q-res))))))

(defgeneric convert-constraints (spec context)
  (:documentation "Converts SPEC into SMT constraints"))

(defun %do-descriptor-name-update (descriptor expr ctx)
  "Updates a descriptor application"
  (make-instance 'smt::expression
                 :name (smt:ensure-identifier
                        (str:concat
                         "ROOT-"
                         (smt:identifier-string
                          descriptor
                          ctx))
                        ctx)
                 :arity (1- (smt:arity expr))
                 :sort (smt:sort expr)
                 :children (rest (smt:children expr))
                 :child-sorts (rest (smt:child-sorts expr))))

(defmethod convert-constraints ((spec spec:relational-specification) context)
  "Converts a relational specification into an SMT constraint"
  (let ((rel (spec:expression spec)))
    (loop for descriptor in (spec:descriptors spec)
          do (setf rel (smt:update-expression
                        (a:curry #'%do-descriptor-name-update descriptor)
                        rel
                        :context context
                        :filter #'(lambda (expr ctx)
                                    (declare (ignore ctx))
                                    (smt:is-application? expr descriptor)))))
    rel))
                                                 

(defmethod convert-constraints ((spec spec:intersection-specification) context)
  "Converts an intersection specification into an SMT constraint"
  (apply #'smt:$and (map 'list (a:rcurry #'convert-constraints context)
                         (spec:components spec))))

(defmethod convert-constraints ((spec spec:union-specification) context)
  "Converts a union specification into an SMT constraint"
  (apply #'smt:$or (map 'list (a:rcurry #'convert-constraints context)
                        (spec:components spec))))
  

(defmethod semgus:verify-program ((verifier concretizing-verifier)
                                  semgus-problem
                                  program
                                  &key produce-cex)
  "Verifies PROGRAM against SEMGUS-PROBLEM"
 
