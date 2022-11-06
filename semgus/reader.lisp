;;;;
;;;; reader for SemGuS problems in declarative s-expression format
;;;;
;;;; (note: this is not the SemGuS format. This is the sexpr parser output format)
(in-package #:com.kjcjohnson.synthkit.semgus)

(defpackage #:com.kjcjohnson.synthkit.semgus-user)

(defvar *semgus-context* nil "Information about problem being parsed")

(defclass semgus-context ()
  ((grammar :accessor grammar
            :initarg :grammar)
   (chcs :accessor chcs
         :initarg :chcs)
   (head-relations :accessor head-relations
                   :initarg :head-relations)
   (constraints :accessor constraints
                :initarg :constraints)
   (root-relation :accessor root-relation
                  :initarg :root-relation)
   (term-name :accessor term-name
              :initarg :term-name)
   (term-type :accessor term-type
              :initarg :term-type)
   (auxiliary-functions :accessor auxiliary-functions
                        :initarg :auxiliary-functions))
  (:default-initargs
   :chcs nil
   :head-relations nil
   :constraints nil
   :auxiliary-functions nil))

(defclass semgus-chc ()
  ((head :accessor head :initarg :head)
   (body :accessor body :initarg :body)
   (constraint :accessor constraint :initarg :constraint)
   (input-variables :accessor input-variables :initarg :input-variables)
   (output-variables :accessor output-variables :initarg :output-variables)
   (variables :accessor variables :initarg :variables)
   (constructor :accessor constructor :initarg :constructor)))

(defclass semgus-chc-head ()
  ((name :accessor name :initarg :name)
   (argument-sorts :accessor argument-sorts :initarg :argument-sorts)
   (term-type :accessor term-type :initarg :term-type)
   (term-index :accessor term-index :initarg :term-index)
   (input-indexes :accessor input-indexes :initarg :input-indexes)
   (output-indexes :accessor output-indexes :initarg :output-indexes)
   (term-name :accessor term-name :initarg :term-name)
   (input-names :accessor input-names :initarg :input-names)
   (output-names :accessor output-names :initarg :output-names)))

(defclass semgus-chc-constructor ()
  ((name :accessor name :initarg :name)
   (arguments :accessor arguments :initarg :arguments)
   (argument-sorts :accessor argument-sorts :initarg :argument-sorts)
   (return-sort :accessor return-sort :initarg :return-sort)))

(defclass semgus-relation ()
  ((name :accessor name :initarg :name)
   (signature :accessor signature :initarg :signature)
   (arguments :accessor arguments :initarg :arguments)))

(defun production-for-chc (chc grammar)
  "Gets the production associated with the CHC in the grammar"
  (find (name (constructor chc))
        (g::productions grammar)
        :test (lambda (name prod)
                ;;(format t "~S : ~S~%" name (g:name (g:operator prod)))
                (eql name (g:name (g:operator prod))))))

(defun load-semgus-problem (filename)
  "Loads a SemGuS problem from the given file."
  (smt:init-smt)

  (let ((*semgus-context* (make-instance 'semgus-context)))
    (with-open-file (stream filename)
      (loop for sexpr = (let ((*package* (find-package "COM.KJCJOHNSON.SYNTHKIT.SEMGUS-USER")))
                          (read stream nil :eof))
            until (eql sexpr :eof)
            doing (eval sexpr)))
    (make-instance 'semgus-problem
                   :specification (constraints-to-pbe)
                   :semantics (make-instance 'default-semantics
                                             :operational
                                             (operationalize-semantics)
                                             :relational nil
                                             :relation-definitions nil)
                   :grammar (grammar *semgus-context*)
                   :context *semgus-context*)))

(defun operationalize-semantics ()
  "Operationalizes semantics - or, at least, tries to."
  (let ((opsem (make-hash-table)))
    (loop for chc in (chcs *semgus-context*)
          for prod = (production-for-chc chc (grammar *semgus-context*))
          doing
             (if (null prod)
                 (warn "No production in grammar for CHC with operator: ~a"
                       (name (constructor chc)))
                 (push
                  (operationalize-chc chc)
                  (gethash (g:operator prod) opsem))))

    #'(lambda (prod)
        (if (null (g:name prod))
            ;; Special case: NT-to-NT productions
            (list #'(lambda (is self child)
                      (declare (ignore self))
                      (funcall child is)))
            (gethash (g:operator prod) opsem)))))

(defun operationalize-chc (chc)
  "Creates a semantic function for a CHC. The result is a function that takes an
input state and semantic functions for each child term"
  (com.kjcjohnson.synthkit.semgus.operationalizer:operationalize-chc+ chc smt:*smt* *semgus-context*))
#|  ;; Note: this is naive and assumes functional semantics
  ;;       we assume the inputs will always be the same as the input state
  (let ((term-output-vars (map 'list
                               #'(lambda (b) (car (last (arguments b))))
                               (body chc)))
        (child-semantic-fns (map 'list
                                 #'(lambda (x) (declare (ignore x)) (gensym "SEM"))
                                 (body chc))))
    (let ((lambda-form
     `(lambda (input-state ,@child-semantic-fns)
        (declare (ignorable input-state))
        (let ,(output-variables chc)
          (declare (ignorable ,@(output-variables chc)))
          (let (,@(map 'list
                       #'(lambda (sem-fn out-var)
                           `(,out-var (smt:get-value
                                       (funcall ,sem-fn input-state)
                                       :output)))
                       child-semantic-fns
                       term-output-vars))
            ,(operationalize-expression (constraint chc)
                                        (input-variables chc)
                                        (output-variables chc)
                                        term-output-vars)
            (values (smt:make-temp-state
                     (list
                      :output
                      ,(first (output-variables chc))))
                    t))))))
      (compile nil lambda-form))))
|#

(defun operationalize-expression (expression input-vars output-vars child-vars &key assigning)
  "Operationalizes a CHC constraint into executable code"
  (cond
    ;; Base cases. Constants, variables, and literals
    ((and (typep expression 'smt::expression)
          (eql (smt:name expression) (smt:ensure-identifier "true")))
     't)
    ((and (typep expression 'smt::expression)
          (eql (smt:name expression) (smt:ensure-identifier "false")))
     'nil)

    ((and (typep expression 'smt::constant)
          (find (smt:name expression) input-vars))
     `(smt:get-value input-state ',(smt:name expression)))

    ((and (typep expression 'smt::constant)
          (or (find (smt:name expression) output-vars)
              (find (smt:name expression) child-vars)))
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
     (let ((arg1 (operationalize-expression (first (smt:children expression))
                                            input-vars
                                            output-vars
                                            child-vars
                                            :assigning t))
           (arg2 (operationalize-expression (second (smt:children expression))
                                            input-vars
                                            output-vars
                                            child-vars
                                            :assigning t)))
       (cond
         ((or (find arg1 output-vars)
              (find arg2 child-vars))
          `(setf ,arg1 ,arg2))
         
         ((or (find arg2 output-vars)
              (find arg1 child-vars))
          `(setf ,arg2 ,arg1))

         ((and (typep arg1 'symbol)
               (not (null arg1))
               (not (eq arg1 t)))
          `(setf ,arg1 ,arg2))
         
         ((and (typep arg2 'symbol)
               (not (null arg2))
               (not (eq arg2 t)))
          `(setf ,arg2 ,arg1))
         (t (error "Tried to do an assignment operator, but not assignable")))))

    ;; Case three: sequenced operations
    ((and (typep expression 'smt::expression)
          (eql (smt:name expression) (smt:ensure-identifier "and")))
     `(progn
        ,@(map 'list #'(lambda (x) (operationalize-expression x
                                                              input-vars
                                                              output-vars
                                                              child-vars))
               (smt:children expression))))

    ;; Case four: known operators
    ((typep expression 'smt::expression)
     (let ((fn (smt:get-compiled-function (smt:name expression))))
       (if fn
           `(funcall ,fn ,@(map 'list #'(lambda (x)
                                          (operationalize-expression x
                                                                     input-vars
                                                                     output-vars
                                                                     child-vars))
                                (smt:children expression)))
           (format *trace-output*
                   "; Missing operational definition for: ~a~%"
                   (smt:name expression)))))

    (t (format *trace-output*
               "; CHC Operationalizer fall-through on: ~a~%"
               expression))))

(defun constraints-to-pbe ()
  "Extracts a PBE specification from the constraints"
  (let ((spec (make-instance 'io-specification)))
    (loop for constraint in (constraints *semgus-context*)
          for exs = (constraint-to-pbe constraint)
          when (null exs) do
            (warn "Unable to convert all constraints to PBE constraints.")
          when (not (null exs)) do
            (let ((input-state (cdr (assoc :inputs exs)))
                  (output-state (cdr (assoc :output exs))))
              (add-example spec
                           (smt:evaluate-state input-state)
                           (smt:evaluate-state output-state)
                           :rel-input input-state
                           :rel-output output-state)))
    spec))

(defun constraint-to-pbe (constraint)
  "Tries to extract a PBE specification from a constraint"
  (let ((appl-name (if (typep constraint 'smt::expression)
                       (smt:name constraint)
                       nil))
        (root-rel (root-relation *semgus-context*))
        (sf-term (term-name *semgus-context*)))
    (cond
      ;; Standard SemGuS-style PBE
      ((eql appl-name (name root-rel))
       ;; Check if we're applying to the synth-fun term
       (let ((termchild (nth (term-index root-rel) (smt:children constraint))))
         (when (and (typep termchild 'smt::expression)
                    (eql (smt:name termchild) sf-term))
           (let ((inputs (smt:make-state
                          (loop for ix in (input-indexes root-rel)
                                for name in (input-names root-rel)
                                collect name
                                collect (nth ix (smt:children constraint)))))
                 (output (smt:make-state
                          (loop for output-ix in (output-indexes root-rel)
                                for output-name in (output-names root-rel)
                                collecting
                                (cons output-name
                                      (elt (smt:children constraint)
                                           output-ix))))))
             (list (cons :inputs inputs) (cons :output output))))))

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
         (let ((inputs (cdr (assoc :inputs (constraint-to-pbe rel-appl)))))
           (when (and (not (null inputs))
                      (typep equality 'smt::expression)
                      (eql (smt:name equality) (smt:ensure-identifier "=")))
             
             (if (eql output-var (smt:name (first (smt:children equality))))
                 (list (cons :inputs inputs)
                       (cons :output
                             (smt:make-state ; SyGuS will only have one output var
                              (list
                               (first (output-names root-rel))
                               (second (smt:children equality))))))
                 (list (cons :inputs inputs)
                       (cons :output
                             (smt:make-state
                              (list
                               (first (output-names root-rel))
                               (first (smt:children equality)))))))))))
       
      
        (t
        nil))))
             
            
       

(defun com.kjcjohnson.synthkit.semgus-user::set-info (name &optional prop)
  "Set info...not well implemented"
  (declare (ignore name prop)))

(defun com.kjcjohnson.synthkit.semgus-user::list (&rest stuff)
  "Creates a list"
  stuff)

(defun com.kjcjohnson.synthkit.semgus-user::identifier (&rest indices)
  "Creates an identifier"
  (smt:ensure-identifier indices))

(defun com.kjcjohnson.synthkit.semgus-user::sort (identifier)
  "Creates a sort"
  (smt:ensure-sort identifier))

(defun com.kjcjohnson.synthkit.semgus-user::declare-term-types (&rest stuff)
  "N/A"
  (declare (ignore stuff)))

(defun com.kjcjohnson.synthkit.semgus-user::add-constructor (&rest stuff)
  "N/A"
  (declare (ignore stuff)))

(defun com.kjcjohnson.synthkit.semgus-user::relation
    (name &key signature arguments)
  "Creates a relation"
  (make-instance 'semgus-relation
                 :name name
                 :signature signature
                 :arguments arguments))

(defun com.kjcjohnson.synthkit.semgus-user::constructor
    (name &key arguments argument-sorts return-sort)
  "Creates a constructor"
  (make-instance 'semgus-chc-constructor
                 :name name
                 :arguments arguments
                 :argument-sorts argument-sorts
                 :return-sort return-sort))

(defun com.kjcjohnson.synthkit.semgus-user::chc
    (&key head body constraint input-variables output-variables variables constructor)
  "Creates a CHC"
  (push
   (make-instance 'semgus-chc
                  :head head
                  :body body
                  :constraint constraint
                  :input-variables input-variables
                  :output-variables output-variables
                  :variables variables
                  :constructor constructor)
   (chcs *semgus-context*))
  (unless (find (name head)
                (head-relations *semgus-context*)
                :test #'(lambda (n r)
                          (eql n (name r))))
    (let ((term-index 0) ;; Shouldn't hardcode this - TODO
          (input-indexes (loop for i from 0
                                         upto (length (signature head))
                                         for arg = (nth i (arguments head))
                                         when (find arg input-variables)
                                           collect i))
          (output-indexes (loop for i from 0
                                         upto (length (signature head))
                                         for arg = (nth i (arguments head))
                                          when (find arg output-variables)
                                           collect i)))

      (push
       (make-instance 'semgus-chc-head
                      :name (name head)
                      :term-index term-index
                      :term-type (nth term-index (signature head))
                      :argument-sorts (signature head)
                      :input-indexes input-indexes 
                      :output-indexes output-indexes
                      :term-name (nth term-index (arguments head))
                      :input-names (map 'list
                                        #'(lambda (i) (nth i (arguments head)))
                                        input-indexes) ;; TOD: warn if no match
                      :output-names (map 'list
                                         #'(lambda (i) (nth i (arguments head)))
                                         output-indexes))
       (head-relations *semgus-context*)))))
                    

;;;
;;; Term handling
;;;
(defun com.kjcjohnson.synthkit.semgus-user::term (term)
  "Wraps a term"
  term)

(defun com.kjcjohnson.synthkit.semgus-user::application
    (name &key argument-sorts arguments return-sort)
  "Creates a function application term"
  (make-instance 'smt::expression
                 :name name
                 :sort return-sort
                 :arity (length arguments)
                 :children arguments
                 :child-sorts argument-sorts))

(defun com.kjcjohnson.synthkit.semgus-user::variable (name &key sort)
  "Creates a variable term"
  (smt:variable name sort))

(defun com.kjcjohnson.synthkit.semgus-user::exists
    (&key bindings binding-sorts child)
  "Creates an existential quantifier"
  (smt::quantifier-expression "exists" bindings binding-sorts child))

(defun com.kjcjohnson.synthkit.semgus-user::forall
    (&key bindings binding-sorts child)
  "Creates a universal quantifier"
  (smt::quantifier-expression "forall" bindings binding-sorts child))

(defun com.kjcjohnson.synthkit.semgus-user::match (&key term binders)
  "Creates a match term"
  (declare (ignore term binders))
  ;; currently not supported
  nil)

(defun com.kjcjohnson.synthkit.semgus-user::binder (&key operator arguments child)
  "Creates a binder for a match clause"
  (declare (ignore operator arguments child))
  ;; currently not supported
  nil)

(defun com.kjcjohnson.synthkit.semgus-user::lambda (&key arguments body)
  (smt::make-lambda-binder arguments body))

;;;
;;; Synthfun and Grammar handling
;;;
(defun com.kjcjohnson.synthkit.semgus-user::production
    (&key instance occurrences operator)
  "Creates a production"
  (append (list #|Name:|#operator instance operator) occurrences))

(defun com.kjcjohnson.synthkit.semgus-user::grammar
    (&key non-terminals productions non-terminal-types)
  "Creates a grammar"
  (declare (ignore non-terminal-types))
  (let ((grammar (g:make-rtg :non-terminals non-terminals
                             :productions productions)))
    grammar))

(defparameter com.kjcjohnson.synthkit.semgus-user::nil nil
  "Nil can appear as an operator in productions that are an NT-to-NT production.")

(defun com.kjcjohnson.synthkit.semgus-user::synth-fun (name &key term-type grammar)
  "Creates a synthesis problem"
  (setf (grammar *semgus-context*) grammar)
  (setf (term-name *semgus-context*) name)
  (setf (term-type *semgus-context*) term-type)
  (let ((possible-root-relations
          (remove-if-not #'(lambda (x) (eql term-type (term-type x)))
                         (head-relations *semgus-context*))))
    (when (endp possible-root-relations)
      (warn "No possible root relations for synth-fun ~a. Make sure CHCs are defined." (smt:identifier-string name)))
    (when (> (length possible-root-relations) 1)
      (warn "More than one possible root relation for synth-fun ~a. One will be chosen 'arbitrarity'." (smt:identifier-string name)))

    (setf (root-relation *semgus-context*) (first possible-root-relations))))

(defun com.kjcjohnson.synthkit.semgus-user::constraint (term)
  "Adds a constraint to the problem"
  (push term (constraints *semgus-context*)))

(defun com.kjcjohnson.synthkit.semgus-user::check-synth ()
  "Finishes the synthesis problem definition"
  nil)

;;;
;;; Function declarations and definitions
;;;
(defun com.kjcjohnson.synthkit.semgus-user::rank (&key argument-sorts return-sort)
  "Creates a function rank"
  (declare (ignore argument-sorts return-sort))
  nil)

(defun com.kjcjohnson.synthkit.semgus-user::declare-function (name &key rank)
  "Adds an auxiliary function declaration"
  (declare (ignore name rank))
  nil)

(defun com.kjcjohnson.synthkit.semgus-user::define-function
    (name &key rank definition)
  "Adds an auxiliary function definition"
  (declare (ignore rank))
  (push (cons name definition) (auxiliary-functions *semgus-context*))
  ;; TODO: maybe, maybe not?
  (smt:set-function-definition name definition)
  nil)

;;;
;;; Datatypes
;;;
(defun com.kjcjohnson.synthkit.semgus-user::declare-datatype (name &key arity)
  "Adds a datatype declaration (with no constructors yet)."
  (declare (ignore arity))
  (setf name (smt:ensure-identifier (smt:name name))) ; NAME comes in as a SORT
  (setf (smt:get-sort name) (make-instance 'smt:datatype :name name)))

(defun com.kjcjohnson.synthkit.semgus-user::add-datatype-constructor
    (&key datatype name children)
  "Adds a constructor to a datatype."
  (let ((constructor (make-instance 'smt:datatype-constructor
                                    :name name
                                    :children children)))
    (unless (typep datatype 'smt:datatype)
      (error "Not a datatype: ~a" datatype))
    (smt:add-datatype-constructor datatype constructor)))
