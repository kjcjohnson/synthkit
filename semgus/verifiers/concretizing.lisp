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

(defmethod semgus:verify-program ((verifier concretizing-verifier)
                                  semgus-problem
                                  program
                                  &key produce-cex)
  "Verifies PROGRAM against SEMGUS-PROBLEM"
 
