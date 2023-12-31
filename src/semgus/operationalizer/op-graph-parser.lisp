;;;;
;;;; Parses into op graphs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.operationalizer)

(defun compute-conjunction-block (conjuncts inputs outputs auxiliaries)
  "Computes an ordering of conjuncts and creates a block of nodes"
  (let ((index-table (%build-index-table conjuncts nil))
        (usage-table (%build-usage-table conjuncts inputs outputs auxiliaries nil)))
    (%apply-usage-table-rule-1 usage-table)

    (unless (%is-usage-table-complete? usage-table)
      (error 'operationalization-error :message "Usage table incomplete."))

    (let* ((node-table (%build-semantic-node-table conjuncts
                                                   nil
                                                   usage-table
                                                   index-table))
           (ordering (%sort-usages usage-table node-table index-table))
           (start nil)
           (curr nil))
      (flet ((without-specials ()
               (loop for x in ordering
                     for s = (%node-semantics (gethash x node-table))
                     unless (member s '(:input :output))
                       collect x)))
        (loop for ix in (without-specials)
              for node = (gethash ix node-table)
              for semantics = (%node-semantics node)
              for inputs = (loop for var in (%node-usages node)
                                 for vt in (%node-usage-types node)
                                 when (eql vt :consumes) collect var)
              for outputs = (loop for var in (%node-usages node)
                                  for vt in (%node-usage-types node)
                                  when (eql vt :produces) collect var)
              for opnode = (parse-block-node semantics inputs outputs nil)
              if (null start)
                do (setf start opnode)
                   (setf curr start)
              else
                do (setf (node-next curr) opnode)
              end
              do (loop until (null (node-next curr))
                       do (setf curr (node-next curr)))))
      start)))

(defun parse-smt-node (semantics inputs outputs)
  "Parses a single SMT node - where the head symbol of SEMANTICS is an SMT function.
This will result in either a guard node or an assignment node, or will thrown an error
if the output sort is not Boolean."
  (cond
    ;;
    ;; Guard nodes. No outputs and Boolean sort
    ;;
    ((null outputs)
     (assert (eql smt:*bool-sort* (smt:sort semantics)))
     (make-instance 'guard-node
                    :inputs inputs
                    :smt semantics))
    ;;
    ;; Assignments. = as the function and one child is the single output variable
    ;;
    ((and (= 1 (length outputs))
          (?:match semantics
            ((?:guard (or (smt:fn "=" ((smt:var o-var) expr))
                          (smt:fn "=" (expr (smt:var o-var))))
                      (eql o-var (first outputs)))
             (make-instance 'assignment-node
                            :smt expr
                            :inputs inputs
                            :outputs outputs)))))

    ;;
    ;; Special case - simple Boolean assignments
    ;;
    ((and (= 1 (length outputs))
          (zerop (length inputs))
          (?:match semantics
            ((?:guard (smt:var vname :sort smt:*bool-sort*)
                      (eql vname (first outputs)))
             (make-instance 'assignment-node
                            :smt (smt:$true)
                            :inputs inputs
                            :outputs outputs))
            ((?:guard (smt:fn "not" ((smt:var vname :sort smt:*bool-sort*)))
                      (eql vname (first outputs)))
             (make-instance 'assignment-node
                            :smt (smt:$false)
                            :inputs inputs
                            :outputs outputs)))))

    ;;
    ;; This shouldn't happen if we classified everything correctly...
    ;;
    (t
     (make-instance 'smt-node
                    :smt semantics
                    :inputs inputs
                    :outputs outputs))))

(defun parse-block-node (semantics inputs outputs auxiliaries)
  "Parses a block node - either a single SMT node, conditionals, or sequences"
  (?:match semantics
    ((smt:fn "or" args)
     (make-instance 'disjunction-node
                    :inputs inputs
                    :outputs outputs
                    :disjuncts (map 'list #'(lambda (s)
                                              (parse-block-node s
                                                                inputs
                                                                outputs
                                                                auxiliaries))
                                    args)))
    ((smt:fn "and" args)
     (compute-conjunction-block args inputs outputs auxiliaries))
    (_
     (parse-smt-node semantics inputs outputs))))
