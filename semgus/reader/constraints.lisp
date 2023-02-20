;;;;
;;;; SemGuS reader functionality
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

;;;
;;; Attempts to derive the most specific specification type for a problem
;;;
;;; 1. If all constraints can be turned into PBE constraints, it
;;;   creates an IO-SPECIFICATION
;;;
;;; 2. If no constraints can be turned into PBE constraints, it
;;;   creates a RELATIONAL-SPECIFICATION
;;;
;;; 3. If some, but not all, constraints can be turned into PBE constraints, it
;;;   creates an INTERSECTION-SPECIFICATION of an IO and relational specification.
;;;

(defun try-derive-pbe-constraint (constraint
                                  &optional (context semgus:*semgus-context*))
  "Attempts to derive a PBE constraint from CONSTRAINT. Returns NIL if not possible."
    (let* ((appl-name (if (typep constraint 'smt::expression)
                        (smt:name constraint)
                        nil))
         (root-rels (semgus:root-relations context))
         (sf-term (semgus:term-name context))
         (appl-root (find appl-name root-rels :test #'eql :key #'semgus:name)))
    (cond
      ;; Standard SemGuS-style PBE
      (appl-root
       ;; Check if we're applying to the synth-fun term
       (let ((termchild (nth (semgus:term-index appl-root) (smt:children constraint))))
         (when (and (typep termchild 'smt::expression)
                    (eql (smt:name termchild) sf-term))
           (let ((inputs (smt:make-state
                          (loop for ix in (semgus:input-indexes appl-root)
                                for name in (semgus:input-names appl-root)
                                collect name
                                collect (nth ix (smt:children constraint)))))
                 (output (smt:make-state
                          (loop for output-ix in (semgus:output-indexes appl-root)
                                for output-name in (semgus:output-names appl-root)
                                collecting
                                (cons output-name
                                      (elt (smt:children constraint)
                                           output-ix))))))
             (list :inputs inputs :output output :descriptor (semgus:name appl-root))))))

      ;; Existentially quantified from SyGuS conversion
      ((and (typep constraint 'smt::quantifier)
            (string= (smt:name constraint) "exists")
            (= 1 (length (smt::arguments constraint)))
            (= 1 (length (smt::children constraint)))
            (typep (first (smt::children constraint)) 'smt::expression)
            (eql (smt:name (first (smt::children constraint)))
                 (smt:ensure-identifier "and")))
       (let ((output-var (first (smt::arguments constraint)))
             (rel-appl (first (smt::children (first (smt::children constraint)))))
             (equality (second (smt::children (first (smt:children constraint))))))
         (let* ((pbe-info (try-derive-pbe-constraint rel-appl context))
                (inputs (getf pbe-info :inputs))
                (descriptor (getf pbe-info :descriptor)))
           (when (and (not (null inputs))
                      (typep equality 'smt::expression)
                      (eql (smt:name equality) (smt:ensure-identifier "=")))

             (let ((root-rel (find descriptor root-rels :test #'eql :key #'semgus:name)))
               (if (eql output-var (smt:name (first (smt:children equality))))
                   (list :descriptor descriptor
                         :inputs inputs
                         :output
                         (smt:make-state ; SyGuS will only have one output var
                          (list
                           (first (semgus:output-names root-rel))
                           (second (smt:children equality)))))
                   (list :descriptor descriptor
                         :inputs inputs
                         :output
                         (smt:make-state
                          (list
                           (first (semgus:output-names root-rel))
                           (first (smt:children equality)))))))))))
       
      
        (t
        nil))))

  
(defun derive-specification (&optional (context semgus:*semgus-context*))
  "Derives a specification from the current semgus CONTEXT"
  (let ((io-spec (make-instance 'semgus:io-specification))
        (rel-spec (list)))
    (loop for constraint in (semgus:constraints context)
          for pbe = (try-derive-pbe-constraint constraint context)
          if pbe do (semgus:add-example io-spec
                                        (getf pbe :inputs)
                                        (getf pbe :output)
                                        (getf pbe :descriptor))
                                        
            else do (push constraint rel-spec))
    (cond ((null rel-spec)
           io-spec)
          ((zerop (semgus:examples-count io-spec))
           (error "Would have returned rel-spec"))
          (t
           (error "Would have returned mixed-spec")))))
