;;;;
;;;; Compiles programs into a single state transformer function
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

(defun %compile-process-calling-card (semantics node cc)
  "Processes a single calling card and returns a transformer function"
  (let* ((children-sem-fns
           (loop for req in (semantics-descriptor-requests cc)
                 for desc = (semantics-descriptor-request-descriptor req)
                 for id = (semantics-descriptor-request-node-id req)
                 if (eql id :self) ; do-compile-and-cache handles the rec call machinery
                   collect (compile-program semantics desc node)
                 else
                   collect (compile-program semantics desc (nth-child id node)))))
    (funcall (semantic-builder-function cc)
             children-sem-fns
             node
             (children node))))

(defun %compile-combine-chc-transformers (chc-transformers)
  "Combines multiple CHC transformer functions into a single transformer"
  (if (> (length chc-transformers) 1)
      (lambda (input-state)
        (declare (type smt:state input-state))
        (declare (optimize (speed 3)))
        (loop for transformer in chc-transformers
              do (multiple-value-bind (result valid)
                     (funcall (the transformer transformer)
                              input-state)
                   (when (or (not (null result)) valid)
                     (return (values result t)))))
        (error "No applicable semantics"))
      (first chc-transformers)))

(defun compile-program (semantics descriptor node)
  "Walks program rooted at NODE and returns a compiled function of one argument, the
input state, that returns the output state computed using SEMANTICS from DESCRIPTOR"
  (do-compile-or-cache (node descriptor)
    (let ((ccs (operational-semantics-for-production semantics
                                                     descriptor
                                                     (production node))))
      (%compile-combine-chc-transformers
       (loop for cc in ccs
             collecting (%compile-process-calling-card semantics node cc))))))
