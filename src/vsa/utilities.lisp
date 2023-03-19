(in-package #:com.kjcjohnson.synthkit.vsa)

(defun ensure-list (thing)
  "Ensures that the thing is a list - if not, wraps it in a one-element list"
  (if (listp thing)
      thing
      (list thing)))

(defun create-most-reasonable-program-node-for-list (programs)
  "Creates the most reasonable program node type for the given list of programs."
  (declare (list programs))
  (let ((count (length programs)))
    (cond
      ((zerop count)
       (make-instance 'empty-program-node))
      ((= 1 count)
       (first programs))
      (t
       (make-instance 'union-program-node :programs programs)))))

(defun cross-inputs-and-descriptors (inputs descriptors)
  "Creates a list of (input . descriptor) pairs"
  (unless descriptors
    (error "Attempt to prune/filter with no descriptors"))
  (loop with results
        for descriptor in (ensure-list descriptors)
        do
           (loop for input in inputs
                 do (push (cons input descriptor) results))
        finally (return results)))

(defun filter (program-node inputs outputs semantics descriptors &key (test #'equal))
  "Filters a program set to only programs that pass the given examples."
  (setf inputs (cross-inputs-and-descriptors inputs descriptors))
  
  (let (programs)
    (do-programs (candidate program-node)
      (when (every #'(lambda (input output)
                       (funcall test (ast:execute-program semantics
                                                          (cdr input)
                                                          candidate
                                                          (car input))
                                output))
                   inputs outputs)
        (push (make-instance 'leaf-program-node :program candidate) programs)))
    (create-most-reasonable-program-node-for-list programs)))

(defun prune (program-enumerable inputs semantics descriptors &key (test #'equal))
  "Prunes programs based on observational equivalence."
  ;; Compute observational equivalence
  (let ((distinct (make-hash-table :test test))
        (input-count 0))
    (dolist (child (ensure-list program-enumerable))
      (do-programs (candidate child)
        (incf input-count)
        (let ((outputs (map 'list #'(lambda (descriptor input)
                                      (unless
                                          (find descriptor
                                                (ast:semantics-descriptors-for-non-terminal
                                                 semantics
                                                 (g:instance
                                                  (ast:production candidate)))
                                                :test #'eql)
                                        (return-from prune
                                          (if (listp program-enumerable)
                                              (make-instance
                                               'union-program-node
                                               :programs program-enumerable)
                                              program-enumerable)))
                                      (ast:execute-program semantics
                                                           descriptor
                                                           candidate
                                                           input))
                            descriptors inputs)))
          (if (not (nth-value 1 (gethash outputs distinct)))
              (setf (gethash outputs distinct) candidate)
              (when
                  (< (ast:program-size candidate)
                     (ast:program-size (gethash outputs distinct)))
                (setf (gethash outputs distinct) candidate))))))
    '(format t "~&;PRUNE IN: ~s OUT: ~s~%" input-count (hash-table-count distinct))
    (create-most-reasonable-program-node-for-list
     (loop for p being the hash-values in distinct
           collecting (make-instance 'leaf-program-node :program p)))))
