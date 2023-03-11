;;;;
;;;; reader for SemGuS problems in declarative s-expression format
;;;;
;;;; (note: this is not the SemGuS format. This is the sexpr parser output format)
(in-package #:com.kjcjohnson.synthkit.semgus)

(defun load-semgus-problem (filename)
  "Loads a SemGuS problem from the given file."
  (smt:init-smt)

  (let ((*semgus-context* (make-instance 'semgus-context)))
    (with-open-file (stream filename)
      (read-problem-from-stream stream *semgus-context*))
    (multiple-value-bind (op-fn desc-map)
        (operationalize-semantics)
      (multiple-value-bind (rel-fn rel-desc-map)
          (process-chcs-for-relational-semantics *semgus-context*)
        (declare (ignore rel-desc-map))
        (make-instance 'semgus-problem
                       :specification (derive-specification *semgus-context*);(constraints-to-pbe)
                       :semantics (make-instance 'default-semantics
                                                 :operational op-fn
                                                 :descriptor-map desc-map
                                                 :relational rel-fn
                                                 :relation-definitions nil)
                       :grammar (grammar *semgus-context*)
                       :context *semgus-context*)))))

(defun operationalize-semantics ()
  "Operationalizes semantics - or, at least, tries to."
  (let ((opsem (make-hash-table))
        (desc-map (make-hash-table)))
    (loop for chc in (chcs *semgus-context*)
          for prod = (chc:production-for-chc chc (grammar *semgus-context*))
          for descriptor = (chc:name (chc:head chc))
          for subtable = (gethash descriptor opsem (make-hash-table))
          doing
             (if (null prod)
                 (warn "No production in grammar for CHC with operator: ~a"
                       (chc:name (chc:constructor chc)))
                 (push
                  (operationalize-chc chc)
                  (gethash (g:operator prod) subtable)))
          unless (null prod)
            do (pushnew descriptor
                        (gethash (g:term-type (g:instance prod)) desc-map))
          end
          do (setf (gethash descriptor opsem) subtable))

    (values
     #'(lambda (descriptor prod)
         (if (null (g:name prod))
             ;; Special case: NT-to-NT productions
             (list (make-instance 'ast:calling-card
                                  :builder-function
                                  #'(lambda (sem-fns node node-children)
                                      (declare (ignore node node-children))
                                      (first sem-fns)) ;; Just return the child fn
                                  :descriptor-requests
                                  (list
                                   (make-instance 'ast:semantics-descriptor-request
                                                  :descriptor descriptor
                                                  :node-id 0))))
             (let ((subtable (gethash descriptor opsem)))
               (unless subtable
                 (error "No semantics for descriptor: ~s" descriptor))
               (gethash (g:operator prod) subtable))))
     desc-map)))

(defun operationalize-chc (chc)
  "Creates a semantic function for a CHC. The result is a function that takes an
input state and semantic functions for each child term"
  (com.kjcjohnson.synthkit.semgus.operationalizer:operationalize-chc+ chc smt:*smt* *semgus-context*))
