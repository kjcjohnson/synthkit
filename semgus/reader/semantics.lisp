;;;;
;;;; Semantics for CHCs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

(defun relationalize-chc (chc context)
  "Creates an SMT relation from a CHC"
  (format t
          "CHC: ~a~% - CONSTRAINT: ~a~% - CONSTRUCTOR: ~a(~{~a~^ ~})~% - PROD: ~a~% - IV: ~a~% - OV: ~a~% - AV: ~a~%~%"
          chc
          (smt:to-smt (semgus:constraint chc))
          (semgus:name (semgus:constructor chc))
          (semgus:arguments (semgus:constructor chc))
          (semgus::production-for-chc chc (semgus:grammar context))
          (semgus:input-variables chc)
          (semgus:output-variables chc)
          (semgus:variables chc))
  (format t "~a~%"
          (smt:to-smt
           (apply #'smt:$and
                  (semgus:constraint chc)
                  (map 'list
                       #'(lambda (b)
                           (make-instance 'smt::expression
                                          :name (semgus:name b)
                                          :arity (length (semgus:arguments b))
                                          :children
                                          (map 'list
                                               #'(lambda (a s)
                                                   (smt:variable a s))
                                               (semgus:arguments b)
                                               (semgus::signature b))
                                          :child-sorts (semgus::signature b)
                                          :sort smt:*bool-sort*))
                       (semgus:body chc))))))

(defmethod semgus:process-chcs-for-relational-semantics (context)
  "Processes a SemGuS context and creates relational semantics"
  (loop for chc in (semgus:chcs context)
        do (relationalize-chc chc context)))
        
