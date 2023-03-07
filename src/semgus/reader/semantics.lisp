;;;;
;;;; Semantics for CHCs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

(defun relationalize-chc (chc context)
  "Creates an SMT relation from a CHC"
  (declare (ignore context))
  chc)

(defmethod semgus:process-chcs-for-relational-semantics (context)
  "Processes a SemGuS context and creates relational semantics"
  (let ((relsem (make-hash-table))
        (descriptor-map (make-hash-table)))
    (loop for chc in (semgus:chcs context)
          for prod = (chc:production-for-chc chc (semgus:grammar context))
          for descriptor = (chc:name (chc:head chc))
          for subtable = (gethash descriptor relsem (make-hash-table :test 'equal))
          doing
             (if (null prod)
                 (warn "No production in grammar for CHC with operator: ~a"
                       (chc:name (chc:constructor chc)))
                 (push
                  (relationalize-chc chc context)
                  (gethash (g:name (g:operator prod)) subtable)))
          unless (null prod)
            do (pushnew descriptor
                        (gethash (g:term-type (g:instance prod)) descriptor-map))
          end
          do (setf (gethash descriptor relsem) subtable))

    (values
     #'(lambda (descriptor production)
         (if (null (g:name production))
             (error "No handling for relational NT-to-NT productions yet")
             (let ((subtable (gethash descriptor relsem)))
               (unless subtable
                 (error "No relational semantics for descriptor: ~s" descriptor))
               (let ((result (gethash (g:name (g:operator production)) subtable)))
                 result))))
     descriptor-map)))
