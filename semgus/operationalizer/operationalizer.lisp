;;;;
;;;; Operationalizer.lisp - converts conjunctions into operational semantics
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.operationalizer)

(defclass uninterpreted-signature ()
  ((inputs  :initarg :inputs  :reader inputs)
   (outputs :initarg :outputs :reader outputs)
   (input-formals :initarg :input-formals :reader input-formals)
   (output-formals :initarg :output-formals :reader output-formals)
   (term-symbol :initarg :term-symbol :reader term-symbol)
   (is-head-input? :initarg :is-head-input? :reader is-head-input?))
  (:default-initargs :is-head-input? nil))

(define-condition operationalization-error ()
  ((message :initarg :message :initform "")))

(defclass %symbol-data ()
  ((name :initarg :name :reader %symbol-name)
   (type :initarg :type :reader %symbol-type) ; :input, :output, :auxiliary
   (usages :initarg :usages :accessor %symbol-usages) ; :produces, :consumes, nil
   (usage-types :initarg :usage-types :accessor %symbol-usage-types))
  (:default-initargs :usages nil :usage-types nil))

(defun %build-index-table (conjuncts child-signatures)
  "Builds index tables between nodes and IDs."
  (let ((forward-index (make-hash-table)) ; ID -> Node
        (reverse-index (make-hash-table)) ; Node -> ID
        (id-counter 0))
    (flet ((add-node (node)
             (let ((id (incf id-counter)))
               (setf (gethash id forward-index) node)
               (setf (gethash node reverse-index) id))))
      (add-node :input)
      (map nil #'add-node conjuncts)
      (map nil #'add-node child-signatures)
      (add-node :output))
    (cons forward-index reverse-index)))

(defun %node->id (node index-table)
  "Gets the ID for a node."
  (gethash node (cdr index-table)))

(defun %id->node (id index-table)
  "Gets the node for an ID."
  (gethash id (car index-table)))

(defun %build-usage-table (conjuncts
                           input-symbols
                           output-symbols
                           auxiliary-symbols
                           child-signatures)
  "Builds a table of usage data for symbols"
  
  (let ((usage-table (make-hash-table :size (+ (length input-symbols)
                                               (length output-symbols)
                                               (length auxiliary-symbols)))))
    (flet ((add-symbol (name type)
             "Adds a symbol to the usage table"
             (setf (gethash name usage-table)
                   (make-instance '%symbol-data :name name :type type)))
           (get-for-sure (name)
             "Gets a symbol from the usage table or throws error if doesn't exist"
             (multiple-value-bind (data found?)
                 (gethash name usage-table)
               (unless found?
                 (error 'operationalization-error
                        :message (format nil "Undeclared variable: ~s" name)))
               data)))
      ;;
      ;; Add initial entries to the data table for each symbol
      ;;
      (dolist (s input-symbols)
        (let ((data (add-symbol s :input)))
          (push :input (%symbol-usages data))
          (push :produces (%symbol-usage-types data))))
      (dolist (s output-symbols)
        (let ((data (add-symbol s :output)))
          (push :output (%symbol-usages data))
          (push :consumes (%symbol-usage-types data))))
      (dolist (s auxiliary-symbols)
        (add-symbol s :auxiliary))
      
      ;;
      ;; Add child signature usages and validate input/outputs
      ;;
      (dolist (c child-signatures)
        (loop for i in (inputs c)
              for data = (get-for-sure i)
              ;; inputs can consume output variables
              doing
                 (push c (%symbol-usages data))
                 (push :consumes (%symbol-usage-types data)))
        (loop for o in (outputs c)
              for data = (get-for-sure o)
              when (eql :input (%symbol-type data)) do
                   (error 'operationalization-error
                          :message (format nil
                                           "~s is an input, but is produced by ~s"
                                           o
                                           c))
              doing
                 (push c (%symbol-usages data))
                 (push :produces (%symbol-usage-types data))))

      ;;
      ;; Add other conjuncts - specifics not known yet
      ;;
      (dolist (conj conjuncts)
        (loop for constant in (smt::find-constants conj)
              for name = (smt:name constant)
              for data = (get-for-sure name)
              doing
                 (push conj (%symbol-usages data))
                 (push nil (%symbol-usage-types data)))))
    ;;
    ;; Remove unused variables
    ;;
    (maphash #'(lambda (name data)
                 (when (null (%symbol-usages data))
                   (remhash name usage-table)))
             usage-table)
    
    usage-table))

(defun %is-usage-table-complete? (usage-table)
  "Checks if the usage table is completely filled out."
  (maphash #'(lambda (name data)
               (declare (ignore name))
               (when (some #'null (%symbol-usage-types data))
                 (return-from %is-usage-table-complete? nil)))
           usage-table)
  t)

(defun %list-replace-all (old new list)
  "Replaces all instances of OLD with NEW in LIST in-place."
  (loop for cell on list
        when (eql old (car cell))
          do (setf (car cell) new)))

(defun %apply-usage-table-rule-1 (usage-table)
  "Rule 1: one and only one usage may produce a variable."
  (maphash #'(lambda (name data)
               (declare (ignore name))
               (let ((type (%symbol-type data))
                     (usage-types (%symbol-usage-types data)))
                 (case type
                   (:input ; All input variable usages are consumers
                    (%list-replace-all nil :consumes usage-types))
                   
                   (:output ; Output variables must have one producer
                    (when (and (zerop (count :produces usage-types))
                               (= 1 (count nil usage-types)))
                      (%list-replace-all nil :produces usage-types)))
                   
                   (:auxiliary ; If a producer exists, the rest are consumers
                    (when (> (count :produces usage-types) 0)
                      (%list-replace-all nil :consumes usage-types))))))
           usage-table))

(defclass %semantic-node ()
  ((identifier :initarg :id :reader %node-identifier)
   (semantics :initarg :semantics :reader %node-semantics)
   (usages :initarg :usages :accessor %node-usages)
   (usage-types :initarg :usage-types :accessor %node-usage-types))
  (:default-initargs :usages nil :usage-types nil))

(defun %build-semantic-node-table (conjuncts
                                   child-signatures
                                   usage-table
                                   index-table)
  "Builds a table of semantic nodes."
  (let ((node-table (make-hash-table)))
    (flet ((add-node (semantics)
             "Adds a semantic node to the table."
             (let ((node (make-instance '%semantic-node
                                        :id (%node->id semantics index-table)
                                        :semantics semantics)))
               (maphash #'(lambda (key value)
                            (let ((upos (position semantics
                                                  (%symbol-usages value))))
                              (unless (null upos)
                                (push key (%node-usages node))
                                (push (elt (%symbol-usage-types value) upos)
                                      (%node-usage-types node)))))
                        usage-table)
               (setf (gethash (%node-identifier node) node-table) node)))

           (add-terminus (which type) ; either :INPUT or :OUTPUT
             (let ((node (make-instance '%semantic-node
                                        :id (%node->id which index-table)
                                        :semantics which)))
               (maphash #'(lambda (key value)
                            (when (eql (%symbol-type value) which)
                              (push key (%node-usages node))
                              (push type (%node-usage-types node))))
                        usage-table)
               (setf (gethash (%node-identifier node) node-table) node))))
      
      (map nil #'add-node conjuncts)
      (map nil #'add-node child-signatures)
      (add-terminus :output :consumes)
      (add-terminus :input :produces))
    node-table))

(defun %graph-all-precedents (graph node)
  "Computes all precedents of a NODE in GRAPH."
  (let ((curr-precedent-list (graph:precedents graph node))
        (prev-precedent-list nil))
    (loop until (null (set-difference curr-precedent-list prev-precedent-list))
          doing
             (setf prev-precedent-list curr-precedent-list)
             (loop for n in prev-precedent-list
                   doing
                      (setf curr-precedent-list
                            (union curr-precedent-list
                                   (graph:precedents graph n)))))
    curr-precedent-list))

(defun %sort-usages (usage-table node-table index-table)
  "Sorts the nodes into operational order."
  (let ((node-graph (make-instance 'graph:digraph)))
    ;; Add nodes to the graph
    (maphash #'(lambda (id node)
                 (declare (ignore node))
                 (graph:add-node node-graph id))
             node-table)

    ;; Add edges
    (maphash #'(lambda (symbol data)
                 ;; For each symbol, add edge from producer to all consumers
                 (let* ((producer-ix (position :produces
                                               (%symbol-usage-types data)))
                        (producer-id (%node->id (elt (%symbol-usages data)
                                                     producer-ix)
                                                index-table)))
                   (loop for node in (%symbol-usages data)
                         for type in (%symbol-usage-types data)
                         when (eql type :consumes) do
                           (graph:add-edge node-graph
                                           (list producer-id
                                                 (%node->id node index-table))
                                           symbol))))
             usage-table)

    ;;
    ;; Handle guards - we need them to sort them as forward as possible
    ;;
    (graph:add-edge node-graph (list (%node->id :output index-table)
                                     (%node->id :input  index-table)))
    (let ((scc (graph:strongly-connected-components node-graph)))
      (graph:delete-edge node-graph (list (%node->id :output index-table)
                                          (%node->id :input  index-table)))
      (let ((critical-path (first (remove-if-not #'(lambda (c)
                                                     (> (length c) 1))
                                                 scc)))
            (guard-nodes (map 'list #'first
                              (remove-if-not #'(lambda (c)
                                                 (= (length c) 1))
                                             scc))))
        ;; What this does:
        ;;   We add a link from every node NOT on the 'critical path'
        ;;   to every node on the critical path that isn't a parent. This
        ;;   will force the non-critical nodes to topo sort as early as possible.
        (dolist (gn guard-nodes)
          (let ((precedents (%graph-all-precedents node-graph gn)))
            (dolist (cpn critical-path)
              (unless (find cpn precedents)
                (graph:add-edge node-graph (list gn cpn))))))
        
        (graph:topological-sort node-graph)))))

(defun %operationalize-expression (expression input-vars output-vars &key assigning)
  "Operationalizes a CHC constraint into executable code"
  (cond
    ;; Base cases. Constants, variables, and literals
    ((and (typep expression 'smt::expression)
          (eql (smt:name expression) (smt:ensure-identifier "true")))
     't)
    ((and (typep expression 'smt::expression)
          (eql (smt:name expression) (smt:ensure-identifier "false")))
     'nil)
    
    ((typep expression 'smt::constant)
     (smt:name expression))

    ((stringp expression)
     expression)
    
    ((numberp expression)
     expression)
    
    ((bit-vector-p expression)
     expression)

    ;; Case two: single assignment
    ((and (not assigning)
          (typep expression 'smt::expression)
          (eql (smt:name expression) (smt:ensure-identifier "=")))
     (let ((arg1 (%operationalize-expression (first (smt:children expression))
                                             input-vars
                                             output-vars
                                             :assigning t))
           (arg2 (%operationalize-expression (second (smt:children expression))
                                             input-vars
                                             output-vars
                                             :assigning t)))
       ;; sets explicitly return T so they aren't treated as guards
       (cond
         ((find arg1 output-vars)
          `(progn
             (setf ,arg1 ,arg2)
             t))
         
         ((find arg2 output-vars)
          `(progn
             (setf ,arg2 ,arg1)
             t))

         (t ; Not an assignment - do an equality check instead
          `(smt::core-= ,arg1 ,arg2)))))

    ;; Case three: sequenced operations
    #|((and (typep expression 'smt::expression)
          (eql (smt:name expression) (smt:ensure-identifier "and")))
     `(progn
        ,@(map 'list #'(lambda (x) (%operationalize-expression x
                                                               input-vars
                                                               output-vars))
               (smt:children expression))))
    |#
    ;; Case four: known operators
    ((typep expression 'smt::expression)
     (let ((fn (smt:get-compiled-function (smt:name expression))))
       (if fn
           `(funcall ,fn ,@(map 'list #'(lambda (x)
                                          (%operationalize-expression x
                                                                      input-vars
                                                                      output-vars))
                                (smt:children expression)))
           (error 'operationalization-error
                  :message
                  (format nil
                          "Missing operational definition for: ~a~%"
                          (smt:name expression))))))

    (t (error 'operationalization-error
              :message
              (format nil
                      "CHC Operationalizer fall-through on: ~a~%"
                      expression)))))


(defun %build-operational-function (input-symbols
                                    output-symbols
                                    auxiliary-symbols
                                    term-symbols
                                    ordering
                                    node-table
                                    index-table
                                    name)
  "Builds an operational definition."
  (with-codegen-data ()
    (let ((lblock (gensym "BLOCK"))
          (is-var (gensym "INPUT"))
          (cs-vars (map 'list #'(lambda (x)
                                  (declare (ignore x))
                                  (gensym "CS"))
                        term-symbols)))
      (with-data-slots
        (setf block-name lblock))
      `(lambda (,is-var ,@cs-vars)
         (declare (ignorable ,is-var ,@cs-vars))
         (declare (optimize (speed 3)))
         (block ,lblock
           ;;
           ;; Declare all variables up front.
           ;; Destructure inputs from the input state.
           ;;
           (let (,@(map 'list #'(lambda (is)
                                  (list is `(smt:get-value ,is-var ',is)))
                        input-symbols)
                 ,@output-symbols
                 ,@auxiliary-symbols)
             (declare (ignorable ,@input-symbols ,@auxiliary-symbols))
             
             ,@(loop for id in ordering
                     for node = (%id->node id index-table)
                     collecting
                     (cond
                       ((eql node :input)
                        `'(SEMANTICS-NAME-------> ,name))
                       ;; No-op.
                       ;; We stuff the name here to be visible in listings.
                       
                       ((eql node :output)
                        
                        `(progn
                           (return-from ,lblock
                             (values (smt:make-temp-state
                                      ,@(map 'list #'(lambda (x)
                                                       `(cons ',x ,x))
                                             output-symbols))
                                     t))))
                       
                       ((typep node 'uninterpreted-signature)
                        (let ((us-pos (position (term-symbol node) term-symbols)))
                          `(multiple-value-bind (output-state successful?)
                               (funcall
                                (the (function (smt:state) smt:state)
                                     ,(elt cs-vars us-pos))
                                ,(if (is-head-input? node)
                                     is-var
                                     `(smt:make-temp-state
                                       ,@(loop for actual
                                                 in (inputs node)
                                               for formal
                                                 in (input-formals node)
                                               collecting
                                               `(cons ',formal
                                                      ,actual)))))
                             ;; Bail if child relation failed
                             (unless successful?
                               (format t "~s   ~s~%" successful? output-state)
                               (return-from ,lblock (values nil nil)))
                             ;; Assign output variables
                             ,@(loop for actual in (outputs node)
                                     for formal in (output-formals node)
                                     collecting
                                     `(setf ,actual
                                            (smt:get-value output-state
                                                           ',formal))))))
                       (t
                        
                        (let ((nd (gethash (%node->id node index-table)
                                           node-table))
                              input-vars
                              output-vars)
                          (loop for symbol in (%node-usages nd)
                                for type in (%node-usage-types nd)
                                when (eql type :consumes)
                                  do (push symbol input-vars)
                                when (eql type :produces)
                                  do (push symbol output-vars))
                          
                          (%codegen-expression-node node
                                                    input-vars
                                                    output-vars)))))))))))
                          
(defun operationalize (conjuncts &key input-symbols
                                   output-symbols
                                   auxiliary-symbols
                                   term-symbols
                                   child-signatures
                                   name)
  "Operationalizes a list of conjuncts. ..."
  
  (let ((index-table (%build-index-table conjuncts
                                         child-signatures))
        (usage-table (%build-usage-table conjuncts
                                         input-symbols
                                         output-symbols
                                         auxiliary-symbols
                                         child-signatures)))
    (%apply-usage-table-rule-1 usage-table)

    (unless (%is-usage-table-complete? usage-table)
      (error 'operationalization-error :message "Usage table incomplete."))

    (let* ((node-table (%build-semantic-node-table conjuncts
                                                  child-signatures
                                                  usage-table
                                                  index-table))
           (ordering (%sort-usages usage-table node-table index-table))

           (opfun (%build-operational-function input-symbols
                                               output-symbols
                                               auxiliary-symbols
                                               term-symbols
                                               ordering
                                               node-table
                                               index-table
                                               name)))
      ;;(format *trace-output* "; GFn: ~&~s~%" opfun)
      (compile nil opfun))))

(defun %extract-conjuncts (formula ctx)
  "Extracts conjuncts as a list from FORMULA, an SMT expression."
  (?:match formula
    ((?:guard (smt::expression (smt:name name) (smt:children children))
              (eql name (smt:ensure-identifier "and" ctx)))
     (apply #'append
            (map 'list #'(lambda (c) (%extract-conjuncts c ctx)) children)))
    (anything (list anything))))

(defun %filter-indices (data-list index-list)
  "Returns only elements from DATA-LIST with indices in INDEX-LIST."
  (map 'list #'(lambda (i) (elt data-list i)) index-list)) ; TODO: not O(kn)

(defun %extract-uninterpreted-signature (relation semgus-ctx)
  "Extracts an uninterpreted signature for a relation."
  ;; Look up the associated CHC head relation
  (let ((head (find (name relation) (head-relations semgus-ctx) :key #'name)))
    (when (null head)
      (error 'operationalization-error
             :message (format nil
                              "Cannot find head relation: ~s"
                              (name relation))))
    (let ((inputs (%filter-indices (arguments relation)
                                   (input-indexes head)))
          (outputs (%filter-indices (arguments relation)
                                    (output-indexes head))))
      (make-instance 'uninterpreted-signature
                     :inputs inputs
                     :outputs outputs
                     :input-formals (input-names head)
                     :output-formals (output-names head)
                     :term-symbol (elt (arguments relation) (term-index head))
                     :is-head-input? (every #'eql inputs (input-names head))))))

(defun operationalize-chc+ (chc smt-ctx semgus-ctx)
  (let* ((is (input-variables chc))
         (os (output-variables chc))
         (head (find (name (head chc)) (head-relations semgus-ctx) :key #'name))
         (ts (cons (term-name head) (arguments (constructor chc)))))
    (operationalize
     (%extract-conjuncts (constraint chc) smt-ctx)
     :name (name (constructor chc))
     :input-symbols is
     :output-symbols os
     :auxiliary-symbols (set-difference (variables chc) (union ts (union is os)))
     :term-symbols ts
     :child-signatures (map 'list #'(lambda (child)
                                      (%extract-uninterpreted-signature
                                       child semgus-ctx))
                            (body chc)))))




#|

(B.Sem t1 x y b) ^ (E.Sem t2 x y r1) ^ (= b true) ^ (= r r1)

x:
B.Sem, E.Sem

y:
B.Sem, E.Sem

b:
B.Sem, b.true

r:
r.r1

r1:
E.Sem, r.r1

|#
  
