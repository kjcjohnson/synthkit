;;;;
;;;; Matchers to map relational constraints to more specific types
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

(defun match-for-universal-constraint (constraint context)
  "Attempts to match a constraint of the form forall x. Sem(t, x) <==> Phi(x)"
  (flet ((fail () (return-from match-for-universal-constraint nil)))
    (?:match constraint
      ((smt:forall bindings
                   (smt:fn "=" ((u:stash semrel (smt:fn semrel-name semrel-args))
                                phi)))
       ;; TODO: not good enough. We have to make sure phi doesn't use the term variable
       (setf semrel (smt:copy-node semrel))
       (a:when-let (rootrel (semgus:lookup-root semrel-name context))
         ;; Relation arguments must only be the forall bindings
         (let (inputs outputs input-sorts output-sorts)
           (loop for actual in semrel-args
                 for ix from 0
                 for sort across (chc:signature rootrel)
                 for role across (chc:roles rootrel)
                 when (eql role :term) do
                   (progn)
                 end
                 when (eql role :input) do
                   (let ((var (smt:name actual)))
                     (if (find var bindings)
                         (progn
                           (push var inputs)
                           (push sort input-sorts))
                         (fail)))
                 end
                 when (eql role :output) do
                   (?:match actual
                     ((smt:var var)
                      (if (find var bindings)
                          (progn
                            (push var outputs)
                            (push sort output-sorts))
                          (fail)))
                     ((or (type bit-vector)
                          (type number)
                          (type string)); TODO: any constant type
                      (let ((temp (smt:unique-identifier)))
                        (push temp outputs)
                        (push sort output-sorts)
                        (setf phi (smt:$and phi
                                            (smt:$= (smt:variable temp sort)
                                                    actual)))
                        (setf (elt (smt:children semrel) ix) (smt:variable temp sort))))
                     (_ (fail)))
                 end)
           (make-instance 'spec:universal-specification
                          :input-symbols (u:ensure-vector inputs)
                          :output-symbols (u:ensure-vector outputs)
                          :input-sorts (u:ensure-vector input-sorts)
                          :output-sorts (u:ensure-vector output-sorts)
                          :constraint phi
                          :descriptors (list semrel-name)
                          :relation semrel
                          :expression constraint)))))))

(defun match-for-existential-constraint (constraint context)
  "Attempts to match a constraint of the form forall i. exists o. Sem(t,i,o) ^ Phi(i,o)"
  (flet ((fail () (return-from match-for-existential-constraint nil)))
    (?:match constraint
      ((smt:forall i-bindings
                   (smt:exists o-bindings
                               (smt:fn "and"
                                       ((u:stash semrel
                                                 (smt:fn semrel-name semrel-args))
                                       phi))))

       (a:when-let (rootrel (semgus:lookup-root semrel-name context))
         ;; Check relation arguments
         (let (inputs outputs input-sorts output-sorts)
           (loop for actual in semrel-args
                 for sort across (chc:signature rootrel)
                 for role across (chc:roles rootrel)
                 for var = (smt:name actual)
                 when (eql role :term) do
                   (progn)
                 end
                 when (eql role :input) do
                   (if (find var i-bindings)
                       (progn
                         (push var inputs)
                         (push sort input-sorts))
                       (fail))
                 end
                 when (eql role :output) do
                   (if (find var o-bindings)
                       (progn
                         (push var outputs)
                         (push sort output-sorts))
                       (fail))
                 end)
           (make-instance 'spec:existential-specification
                          :input-symbols (u:ensure-vector inputs)
                          :output-symbols (u:ensure-vector outputs)
                          :input-sorts (u:ensure-vector input-sorts)
                          :output-sorts (u:ensure-vector output-sorts)
                          :constraint phi
                          :descriptors (list semrel-name)
                          :relation semrel
                          :expression constraint)))))))
