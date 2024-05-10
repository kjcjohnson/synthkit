;;;;
;;;; SemGuS reader functionality
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

;;;
;;; We should put together a better core config mechanism at some point...
;;;
(defparameter *force-no-pbe-constraints* nil "Set to not produce PBE constraints")

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
  (when *force-no-pbe-constraints*
    (return-from try-derive-pbe-constraint nil))

  (let* ((appl-name (if (typep constraint 'smt:application)
                        (smt:name constraint)
                        nil))
         (root-rels (semgus:root-relations context))
         (sf-term (semgus:term-name context))
         (appl-root (find appl-name root-rels :test #'eql :key #'chc:name)))
    (cond
      ;; Standard SemGuS-style PBE
      (appl-root
       ;; Check if we're applying to the synth-fun term
       (let ((termchild (nth (chc:term-index appl-root) (smt:children constraint))))
         (when (and (typep termchild 'smt:application)
                    (eql (smt:name termchild) sf-term))
           (let ((inputs (smt:make-state
                          (loop for ix in (chc:input-indices appl-root)
                                for name in (chc:input-formals appl-root)
                                collect name
                                collect (nth ix (smt:children constraint)))))
                 (output (smt:make-state
                          (loop for output-ix in (chc:output-indices appl-root)
                                for output-name in (chc:output-formals appl-root)
                                collecting
                                (cons output-name
                                      (elt (smt:children constraint)
                                           output-ix))))))
             (list :inputs inputs :output output :descriptor (chc:name appl-root))))))

      ;; Existentially quantified from SyGuS conversion
      ((and (typep constraint 'smt:quantifier)
            (string= (smt:name constraint) "exists")
            (= 1 (length (smt:arguments constraint)))
            (= 1 (length (smt:children constraint)))
            (typep (first (smt:children constraint)) 'smt:application)
            (eql (smt:name (first (smt:children constraint)))
                 (smt:ensure-identifier "and")))
       (let ((output-var (first (smt:arguments constraint)))
             (rel-appl (first (smt:children (first (smt:children constraint)))))
             (equality (second (smt:children (first (smt:children constraint))))))
         (let* ((pbe-info (try-derive-pbe-constraint rel-appl context))
                (inputs (getf pbe-info :inputs))
                (descriptor (getf pbe-info :descriptor)))
           (when (and (not (null inputs))
                      (typep equality 'smt:application)
                      (eql (smt:name equality) (smt:ensure-identifier "=")))

             (let ((root-rel (find descriptor root-rels :test #'eql :key #'chc:name)))
               (if (eql output-var (smt:name (first (smt:children equality))))
                   (list :descriptor descriptor
                         :inputs inputs
                         :output
                         (smt:make-state ; SyGuS will only have one output var
                          (list
                           (first (chc:output-formals root-rel))
                           (second (smt:children equality)))))
                   (list :descriptor descriptor
                         :inputs inputs
                         :output
                         (smt:make-state
                          (list
                           (first (chc:output-formals root-rel))
                           (first (smt:children equality)))))))))))


        (t
        nil))))

(defun %derive-relational-specification-for-constraint (constraint context)
  "Gets the most specific relational specification possible for CONSTRAINT."
  (a:if-let (s (match-for-universal-constraint constraint context))
    s
    (a:if-let (s (match-for-existential-constraint constraint context))
      s
      (make-instance 'spec:relational-specification
                     :expression constraint
                     :descriptors (%extract-descriptors constraint context)))))

(defun %extract-descriptors (expression context)
  "Extracts all descriptors used in an SMT expression."
  (loop for root-rel in (semgus:root-relations context)
        when (smt:map-expression (constantly t)
                                 expression
                                 :filter (a:rcurry #'smt:is-application?
                                                   (chc:name root-rel))
                                 :join (a:curry #'some #'identity))
          collect (chc:name root-rel)))

(defun derive-specification-for-constraint (constraint context)
  "Derives a specification from an individual constraint clause"
  ;; Try PBE
  (let ((pbe (try-derive-pbe-constraint constraint context)))
    (if pbe
        (make-instance 'spec:io-specification
                       :input-state (smt:evaluate-state (getf pbe :inputs))
                       :output-state (smt:evaluate-state (getf pbe :output))
                       :descriptor (getf pbe :descriptor))
        (%derive-relational-specification-for-constraint constraint context))))

(defmethod semgus:derive-specification (context)
  "Derives a specification from the current semgus CONTEXT"
  (let ((specs
          (loop for constraint in (semgus:constraints context)
                collecting (derive-specification-for-constraint constraint context))))
    (cond
      ((null specs)
       (error "No specifications found!"))
      ((= 1 (length specs))
       (first specs))
      (t
       (make-instance 'spec:intersection-specification :components specs)))))
