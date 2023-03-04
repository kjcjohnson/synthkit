;;;;
;;;; Traversing SMT expressions
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defun map-expression (fn expression &key join (filter (constantly t)))
  "Calls FN on all nodes of EXPRESSION, optionally matching FILTER."
  (labels ((map-expression-rec (child)
             "Recursive call to MAP-EXPRESSION"
             (map-expression fn child :join join :filter filter)))

    (let ((node-result (when (funcall filter expression)
                         (funcall fn expression)))
          ;; TODO: LET and CASE will probably need special handling
          (child-result
            (when (typep expression 'expression)
              (if join
                  (funcall join (map 'list #'map-expression-rec (children expression)))
                  (map nil #'map-expression-rec (children expression))))))
      (funcall join (list node-result child-result)))))

(defun %update-expression-td (fn expression filter context)
  "Does top-down updating. Returns new node, and T if any values changed"
  (let ((node expression))
    (when (funcall filter expression context)
      (setf node (funcall fn expression context)))
    
    (when (typep node 'expression)
      (let* ((children-changed? nil)
             (children
               (loop with new-child and changed?
                     for child in (children node)
                     do (setf (values new-child changed?)
                              (%update-expression-td fn child filter context))
                     collect new-child
                     do (setf children-changed? (or children-changed? changed?)))))

        (when children-changed?
          (setf node (copy-node node :children children)))))
    
    (values node (not (eql node expression)))))

(defun update-expression (fn expression &key (filter (constantly t)) (from-top t) (context *smt*))
  "Calls FN to update SMT nodes in the tree, optionally matching FILTER. If FROM-TOP is
T, then FN is called before a node's children are traversed - the node returned by FN
will have its children traversed."
  (if from-top
      (%update-expression-td fn expression filter context)
      (error "Bottom-up traversal not implemented")))
  

