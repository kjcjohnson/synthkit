;;;;
;;;; Semantics for CHCs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

(defun relationalize-chc (chc context)
  "Creates an SMT relation from a CHC"
  (format t
          "CHC: ~a~% - CONSTRAINT: ~a~% - CONSTRUCTOR: ~a(~{~a~^ ~})~% - PROD: ~a~% - IV: ~a~% - OV: ~a~% - AV: ~a~%~%"
          chc
          (smt:to-smt (chc:constraint chc))
          (chc:name (chc:constructor chc))
          (chc:arguments (chc:constructor chc))
          (semgus::production-for-chc chc (semgus:grammar context))
          (map 'list #'chc:symbol-name (chc:input-symbols chc))
          (map 'list #'chc:symbol-name (chc:output-symbols chc))
          (map 'list #'chc:symbol-name (chc:auxiliary-symbols chc)))
  (format t "~a~%"
          (smt:to-smt
           (apply #'smt:$and
                  (chc:constraint chc)
                  (map 'list
                       #'(lambda (b)
                           (make-instance 'smt::expression
                                          :name (chc:name b)
                                          :arity (length (chc:actuals b))
                                          :children
                                          (map 'list
                                               #'(lambda (a s)
                                                   (smt:variable a s))
                                               (chc:actuals b)
                                               (chc:signature b))
                                          :child-sorts (chc:signature b)
                                          :sort smt:*bool-sort*))
                       (chc:body chc))))))

(defmethod semgus:process-chcs-for-relational-semantics (context)
  "Processes a SemGuS context and creates relational semantics"
  (loop for chc in (semgus:chcs context)
        do (relationalize-chc chc context)))
        
