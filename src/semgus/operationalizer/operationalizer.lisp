;;;;
;;;; Operationalizer.lisp - converts conjunctions into operational semantics
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.operationalizer)

(defvar *default-optimization* '((speed 3)) "Optimization setting for functions")

(defclass uninterpreted-signature ()
  ((inputs  :initarg :inputs  :reader inputs)
   (outputs :initarg :outputs :reader outputs)
   (input-formals :initarg :input-formals :reader input-formals)
   (output-formals :initarg :output-formals :reader output-formals)
   (term-symbol :initarg :term-symbol :reader term-symbol)
   (head-symbol :initarg :head-symbol :reader head-symbol)
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

(defun pprint-usage-table (stream usage-table)
  "Pretty prints a usage table hash table"
  (flet ((print-usage (usage)
           "Makes a better attempt at readable usages"
           (typecase usage
             (symbol usage)
             (uninterpreted-signature "child")
             (smt::expression (smt:to-smt usage :pprint t))
             (otherwise "other"))))
    (format stream "~&--Usage Table--~%")
    (let ((left-max 0))
      (maphash #'(lambda (name data)
                   (declare (ignore data))
                   (setf left-max (max left-max (length (smt:identifier-string name)))))
               usage-table)
      (maphash #'(lambda (name data)
                   (format stream "~va [~a]~%"
                           left-max
                           (smt:identifier-string name)
                           (%symbol-type data))
                   (loop for usage in (%symbol-usages data)
                         for type in (%symbol-usage-types data)
                         do
                            (format stream
                                    " ~a ~a~%"
                                    (case type
                                      (:consumes "consumed by:")
                                      (:produces "produced by:")
                                      (otherwise "****NIL****:"))
                                    (print-usage usage)))
                   (format stream "---------------~%"))
               usage-table))))

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

(defun %apply-usage-table-rule-2 (usage-table)
  "Rule 2: one and only one usage must produce a variable"
  (maphash #'(lambda (name data)
               (declare (ignore name))
               (let ((usage-types (%symbol-usage-types data)))
                 (cond
                   ((= 1 (count :produces usage-types))
                    (%list-replace-all nil :consumes usage-types))

                   ((and (zerop (count :produces usage-types))
                         (= 1 (count nil usage-types)))
                    (%list-replace-all nil :produces usage-types)))))
           usage-table))

(defun %apply-usage-table-rule-3 (usage-table)
  "Rule 3: try a little harder for auxiliaries that don't have a producer"
  (maphash #'(lambda (name data)
               (when (and (eql (%symbol-type data) :auxiliary)
                          (not (find :produces (%symbol-usage-types data))))
                 (loop for usage in (%symbol-usages data)
                       for typecdr on (%symbol-usage-types data)
                       when (?:match usage
                              ((?:guard (smt:fn "=" ((smt:var x) _))
                                        (eql x name))
                               t))
                         do (progn
                              (setf (car typecdr) :produces)
                              (%list-replace-all nil :consumes
                                                 (%symbol-usage-types data))
                              (loop-finish)))))
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

    ;; Constants done later

    ((stringp expression)
     expression)

    ((numberp expression)
     expression)

    ((bit-vector-p expression)
     expression)

    ((smt:native-literal? expression)
     (smt:native-value expression))

    ((smt:native-break? expression)
     `(progn
        (when (funcall ,(smt:native-break-condition expression))
          (break))
        t))

    ;; Case 2a: single assignment
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

    ;; Case 2b: Single Boolean variable assertions
    ((and (not assigning)
          (zerop (length input-vars))
          (= 1 (length output-vars))
          (?:match expression
            ((?:guard (smt:var vname :sort smt:*bool-sort*)
                      (eql vname (first output-vars)))
             `(progn
                (setf ,(first output-vars) t)
                t))
            ((?:guard (smt:fn "not" ((smt:var vname :sort smt:*bool-sort*)))
                      (eql vname (first output-vars)))
             `(progn
                (setf ,(first output-vars) nil)
                t)))))

    ;; Case 2c: Regular constants that aren't outputs - use their value
    ((typep expression 'smt::constant)
     (smt:name expression))

    ;; Case 3a: Conditional sequences: Run all, but stop running when the first
    ;;          one returns NIL. Note that the one that returns NIL will not have
    ;;          its side effects undone!
    ((and (not assigning)
          (typep expression 'smt::expression)
          (or (eql (smt:name expression) (smt:ensure-identifier "and"))
              (string= (smt:name expression) "and")))
     `(and
        ,@(map 'list #'(lambda (x) (%operationalize-expression x
                                                               input-vars
                                                               output-vars))
               (smt:children expression))))

    ;; Case 3b: Conditional sequences. Run all, up until one returns T
    ;;          This is sort of sketchy, because the side effects of any that
    ;;          return NIL will not be undone, and we don't attempt any sort
    ;;          of reordering operation. Ideally, we should put guards first!
    ((and (not assigning)
          (typep expression 'smt::expression)
          (or
           (eql (smt:name expression) (smt:ensure-identifier "or"))
           (string= (smt:name expression) "or"))) ; :/
     `(or
        ,@(map 'list #'(lambda (x) (%operationalize-expression x
                                                               input-vars
                                                               output-vars))
               (smt:children expression))))

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

(defun %build-operationalization-graph (input-symbols
                                        output-symbols
                                        auxiliary-symbols
                                        ordering
                                        node-table
                                        index-table
                                        name
                                        child-signatures
                                        term-symbols)
  (declare (ignore auxiliary-symbols index-table child-signatures))
  (flet ((only-of-type (vars types type)
           "Returns only variables in VARS with TYPE in TYPES"
           (loop for var in vars
                 for vt in types
                 when (eql vt type) collect var)))
    (let* ((start (make-instance 'begin-node :name name :terms term-symbols))
           (curr start))
      (loop for ix in ordering
            for node = (gethash ix node-table)
            for semantics = (%node-semantics node)
            for inputs = (only-of-type (%node-usages node)
                                       (%node-usage-types node)
                                       :consumes)
            for outputs = (only-of-type (%node-usages node)
                                        (%node-usage-types node)
                                        :produces)
            do
               (cond
                 ((eql :input semantics)
                  (setf (node-next curr) (make-instance 'input-node
                                                        :outputs input-symbols)))
                 ((eql :output semantics)
                  (setf (node-next curr)
                        (make-instance 'output-node
                                       :inputs output-symbols)))
                 ((typep semantics 'uninterpreted-signature)
                  (setf (node-next curr)
                        (make-instance 'child-node
                                       :descriptor (head-symbol semantics)
                                       :term (term-symbol semantics)
                                       :inputs (only-of-type (%node-usages node)
                                                             (%node-usage-types node)
                                                             :consumes)
                                       :outputs (only-of-type (%node-usages node)
                                                              (%node-usage-types node)
                                                              :produces))))

                 (t
                  (setf (node-next curr)
                        (parse-block-node semantics inputs outputs nil))))
            do (loop until (null (node-next curr))
                     do (setf curr (node-next curr))))
      (setf (node-next curr) (make-instance 'end-node :next nil))
      ;;(format t "------------------------------------------------------------~%")
      ;;(pprint-opgraph t start)
      start)))

(defun %build-operational-function (input-symbols
                                    output-symbols
                                    auxiliary-symbols
                                    ordering
                                    node-table
                                    index-table
                                    name
                                    child-signatures)
  "Builds an operational definition."
  (with-codegen-data ()
    (let ((lblock (gensym "BLOCK"))
          (is-var (gensym "INPUT"))
          (cs-var (gensym "CS")))
      (with-data-slots
        (setf block-name lblock))
      `(lambda (,is-var ,cs-var)
         (declare (ignorable ,is-var ,cs-var))
         (declare (optimize ,@*default-optimization*))
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
                        (let ((us-pos (position node child-signatures)))
                          `(multiple-value-bind (output-state successful?)
                               (funcall
                                (the (function (smt:state) smt:state)
                                     (elt (the list ,cs-var) ,us-pos))
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
                               (format t
                                       "~s  ~s -> ~s [~s]~%"
                                       successful?
                                       ,is-var
                                       output-state
                                       ,(nth us-pos child-signatures))
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
    ;; Do we need to run these until we reach a fixpoint?
    (%apply-usage-table-rule-1 usage-table)
    (%apply-usage-table-rule-2 usage-table)
    (%apply-usage-table-rule-3 usage-table)

    (unless (%is-usage-table-complete? usage-table)
      (pprint-usage-table *trace-output* usage-table)
      (error 'operationalization-error :message "Usage table incomplete."))

    (let* ((node-table (%build-semantic-node-table conjuncts
                                                   child-signatures
                                                   usage-table
                                                   index-table))
           (ordering (%sort-usages usage-table node-table index-table))
           (opgraph (%build-operationalization-graph input-symbols
                                                     output-symbols
                                                     auxiliary-symbols
                                                     ordering
                                                     node-table
                                                     index-table
                                                     name
                                                     child-signatures
                                                     term-symbols))
           (opfun (%build-operational-function input-symbols
                                               output-symbols
                                               auxiliary-symbols
                                               ordering
                                               node-table
                                               index-table
                                               name
                                               child-signatures)))
      ;;(format *trace-output* "; GFn: ~&~s~%" opfun)
      (let ((semfn (compile nil opfun)))
        (make-instance 'ast:calling-card
                       :builder-function
                       #'(lambda (sem-fns node node-children)
                           (declare (ignore node node-children))
                           #'(lambda (input-state)
                               (multiple-value-bind (result valid?)
                                   (funcall semfn input-state sem-fns)
                                 (values result valid?))))
                       :descriptor-requests
                       (map 'list
                            #'(lambda (sig)
                                (make-instance 'ast:semantics-descriptor-request
                                               :node-id
                                               (let ((id
                                                       (position (term-symbol sig)
                                                                 term-symbols)))
                                                 (if (zerop id)
                                                     :self
                                                     (1- id)))
                                               :descriptor
                                               (head-symbol sig)))
                            child-signatures))))))

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
  (declare (ignore semgus-ctx))
  (let ((head (chc:head relation)))
    (assert (not (chc:is-forward-declared-head? head)))
    (when (null head)
      (error 'operationalization-error
             :message (format nil
                              "Cannot find head relation: ~s"
                              (chc:name relation))))
    (let ((inputs-a (chc:filter-role :input (chc:actuals relation) (chc:roles head)))
          (outputs-a (chc:filter-role :output (chc:actuals relation) (chc:roles head)))
          (inputs-f (chc:filter-role :input (chc:formals head) (chc:roles head)))
          (outputs-f (chc:filter-role :output (chc:formals head) (chc:roles head)))
          (term-a (chc:filter-role :term (chc:actuals relation) (chc:roles head))))

      (make-instance 'uninterpreted-signature
                     :inputs inputs-a
                     :outputs outputs-a
                     :input-formals inputs-f
                     :output-formals outputs-f
                     :term-symbol (first term-a)
                     :head-symbol (chc:name head)
                     :is-head-input? (every #'eql inputs-a inputs-f)))))

(defmethod semgus:operationalize-chc (chc smt-ctx semgus-ctx)
  (let* ((symbol-table (chc:symbol-table chc))
         (is (map 'list #'chc:symbol-name (chc:input-symbols symbol-table)))
         (os (map 'list #'chc:symbol-name (chc:output-symbols symbol-table)))
         (as (map 'list #'chc:symbol-name (chc:auxiliary-symbols symbol-table)))
         (ts (cons (chc:symbol-name (chc:term-symbol symbol-table))
                   (map 'list #'chc:symbol-name (chc:child-symbols symbol-table)))))

    (operationalize
     (%extract-conjuncts (chc:constraint chc) smt-ctx)
     :name (chc:name (chc:constructor chc))
     :input-symbols is
     :output-symbols os
     :auxiliary-symbols as
     :term-symbols ts
     :child-signatures (map 'list #'(lambda (child)
                                      (%extract-uninterpreted-signature
                                       child semgus-ctx))
                            (chc:body chc)))))




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
