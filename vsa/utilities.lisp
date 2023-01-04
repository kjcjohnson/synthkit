(in-package #:com.kjcjohnson.synthkit.vsa)
(kl/oo:import-classes-from #:kl/c)
(kl/oo:import-classes-from #:com.kjcjohnson.synthkit.vsa)

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
       (empty-program-node:new))
      ((= 1 count)
       (first programs))
      (t
       (union-program-node:new programs)))))

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
    (kl:foreach (candidate in program-node)
      (when (every #'(lambda (input output)
                       (funcall test (ast:execute-program semantics
                                                          (cdr input)
                                                          candidate
                                                          (car input))
                                output))
                   inputs outputs)
        (push (leaf-program-node:new candidate) programs)))
    (create-most-reasonable-program-node-for-list programs)))

(defun prune (program-enumerable inputs semantics descriptors &key (test #'equal))
  "Prunes programs based on observational equivalence."
  ;; Compute observational equivalence
  (let ((distinct (dictionary:new :test test))
        (input-count 0))
    (kl:foreach (child in (ensure-list program-enumerable))
      (kl:foreach (candidate in child)
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
                                              (union-program-node:new
                                               program-enumerable)
                                              program-enumerable)))
                                      (ast:execute-program semantics
                                                           descriptor
                                                           candidate
                                                           input))
                            descriptors inputs)))
          (if (not (&dictionary:contains-key distinct outputs))
              (&dictionary:add distinct outputs candidate)
              (when
                  (< (ast:program-size candidate)
                     (ast:program-size (&dictionary:try-get-value
                                        distinct
                                        outputs)))
                (&dictionary:add distinct outputs candidate))))))
    '(format t "~&;PRUNE IN: ~s OUT: ~s~%" input-count (length (&dictionary:value-list distinct)))
    (create-most-reasonable-program-node-for-list
     (map 'list
          #'(lambda (p)
              (leaf-program-node:new p))
          (&dictionary:value-list distinct)))))
