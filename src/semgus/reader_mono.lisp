;;;;
;;;; reader for SemGuS problems in declarative s-expression format
;;;;
;;;; (note: this is not the SemGuS format. This is the sexpr parser output format)
(in-package #:com.kjcjohnson.synthkit.semgus)

(defun load-semgus-problem (filename)
  "Loads a SemGuS problem from the given file."
  (smt:init-smt)

  (let ((*semgus-context* (make-instance 'semgus-context
                                         :path (pathname filename))))
    (with-open-file (stream filename)
      (read-problem-from-stream stream *semgus-context*))
    (call-context-load-processors *semgus-context*)
    (multiple-value-bind (op-fn desc-map)
        (operationalize-semantics)
      (multiple-value-bind (rel-fn rel-desc-map)
          (process-chcs-for-relational-semantics *semgus-context*)
        (declare (ignore rel-desc-map))
        (let ((problem
                (make-instance 'semgus-problem
                               :specification (derive-specification *semgus-context*)
                               :semantics (make-instance 'default-semantics
                                                         :operational op-fn
                                                         :descriptor-map desc-map
                                                         :relational rel-fn
                                                         :relation-definitions nil)
                               :grammar (grammar *semgus-context*)
                               :context *semgus-context*)))
          (call-problem-load-processors problem)
          problem)))))

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
                  (operationalize-chc chc smt:*smt* *semgus-context*)
                  (gethash (g:operator prod) subtable)))
          unless (null prod)
            do (pushnew descriptor
                        (gethash (g:term-type (g:instance prod)) desc-map))
          end
          do (setf (gethash descriptor opsem) subtable))

    (call-operationalization-load-processors opsem desc-map *semgus-context*)

    (values
     #'(lambda (descriptor prod)
         (if (typep prod 'g:production)
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
                   (gethash (g:operator prod) subtable)))
             (let ((subtable (gethash descriptor opsem)))
               (unless subtable
                 (error "No auxiliary semantics for descriptor: ~s and: ~a"
                        descriptor prod))
               (gethash prod subtable))))
     desc-map)))
