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
  
(defun filter (program-node inputs outputs semantics &key (test #'equal))
  "Filters a program set to only programs that pass the given examples."
  (let (programs)
    (kl:foreach (candidate in program-node)
      (when (every #'(lambda (input output)
                       (funcall test (ast:execute-program semantics
                                                          candidate
                                                          input)
                                output))
                   inputs outputs)
        (push (leaf-program-node:new candidate) programs)))
    (create-most-reasonable-program-node-for-list programs)))

(defun prune (program-enumerable inputs semantics &key (test #'equal))
  "Prunes programs based on observational equivalence."
  '(format *trace-output* "~&;;     Before Prune --> ~s programs ~a ~~~~> ~a~%"
    (reduce #'+ (map 'list #'(lambda (p)
                               (program-node:program-count p))
                 children))
    inputs outputs)
  ;; Compute observational equivalence
  (let ((distinct (dictionary:new :test test)))
    (kl:foreach (child in (ensure-list program-enumerable))
      (kl:foreach (candidate in child)
        (let ((outputs (map 'list #'(lambda (input)
                                      (ast:execute-program semantics
                                                           candidate
                                                           input))
                            inputs)))
          (if (not (&dictionary:contains-key distinct outputs))
              (&dictionary:add distinct outputs candidate)
              (when
                  (< (ast:program-size candidate)
                     (ast:program-size (&dictionary:try-get-value
                                        distinct
                                        outputs)))
                (&dictionary:add distinct outputs candidate))))))
    '(format *trace-output* "~&;;      After Prune --> ~s programs ~a ~~~~> ~a~%"
      (length (&dictionary:value-list distinct)) inputs outputs)
    (create-most-reasonable-program-node-for-list
     (map 'list
          #'(lambda (p)
              (leaf-program-node:new p))
          (&dictionary:value-list distinct)))))
