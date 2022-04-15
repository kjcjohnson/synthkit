;;;
;;; ast.lisp - functionality for program nodes and other ast stuff
;;;
(in-package #:com.kjcjohnson.synthkit.ast)

(defclass program-node ()
  ((operator
    :initarg :operator
    :reader operator
    :documentation "Operator for this AST node.")
   (production
    :initarg :production
    :reader production
    :documentation "Production for this AST node.")
   (children
    :initarg :children
    :initform (list)
    :accessor children
    :documentation "Children of this AST node.")
   #+(or)(operational-semantics
    :initarg :operational-semantics
    :initform nil
    :accessor operational-semantics
    :documentation "This node's operational semantics - a list of state transformer functions")))

(defmethod initialize-instance :after ((n program-node) &key)
  ;; Must have either operator or production set
  (assert (or (slot-boundp n 'operator) (slot-boundp n 'production)))
  (when (not (slot-boundp n 'operator))
    (setf (slot-value n 'operator) (g:operator (slot-value n 'production))))
  (if (not (slot-boundp n 'production))
      (setf (slot-value n 'production) nil)
      (assert (eql (slot-value n 'operator) (g:operator (slot-value n 'production)))))
  (assert (= (g:arity (slot-value n 'operator)) (length (slot-value n 'children)))))

(defvar *printing-program-node* nil)

(defgeneric print-program-operator (op children stream))

(defmethod print-program-operator (op children stream)
  (format stream "~a" (g:name op))
  (unless (null children)
    (format stream "(~{~a~^,~})" children)))

(defgeneric print-program-node (n stream))

(defmethod print-program-node (n stream)
  (print-program-operator (operator n) (children n) stream))

(defmethod print-object ((n program-node) stream)
  (if *printing-program-node*
      (print-program-node n stream)
      (let ((*printing-program-node* t))
        (print-unreadable-object (n stream :type t)
          (print-program-node n stream)))))

(defun copy-program (program)
  "Returns a copy of the given program node."
  (make-instance 'program-node :production (production program)
                               :operator (operator program)
                               :children (map 'list #'copy-program (children program))))

(defgeneric nth-child (n thing)
  (:documentation "Retrieves the nth child of the thing"))

(defgeneric (setf nth-child) (value n thing)
  (:documentation "Sets the nth child of the thing"))

(defmethod nth-child (n (thing program-node))
  (nth n (children thing)))

(defmethod (setf nth-child) (value n (thing program-node))
  (setf (nth n (children thing)) value))

(defmacro swap-nth-child (place n thing)
  `(psetf ,place (nth-child ,n ,thing)
          (nth-child ,n ,thing) ,place))

;; (defmacro dochildren (var node &body body)
;;   "Calls FN on all children of NODE, recursively"
;;   (dolist (c (children node))
    
(defun program-size (node)
  "Computes the size of NODE (number of nodes in the tree)"
  (1+ (reduce #'+ (map 'list #'program-size (children node)))))

(defgeneric operational-semantics-for-production (semantics production node)
  (:documentation "Maps from a production to a list of state transformation semantic functions."))

(defgeneric relational-semantics-for-production (semantics production)
  (:documentation "Maps from a production to CHC-based relational semantics."))

(defgeneric relational-semantics-for-non-terminal (semantics non-terminal)
  (:documentation "Maps from a non-terminal to the semantics relation function declaration."))

(defvar *program-execution-exit-hook*)
(defun abort-program-execution ()
  (funcall *program-execution-exit-hook*))

(defun compile-program (node semantics)
  (with-slots (production) node
    (when (null production) (error "Cannot compile a program node without a production."))
    (let ((input-state-var (gensym "INPUT"))
          (output-state-var (gensym "OUTPUT")))
      `(lambda (,input-state-var)
         ,(dolist (s (operational-semantics-for-production semantics production node))
            `(let ((,output-state-var (funcall ,s ,input-state-var)))
               (unless (null ,output-state-var) (return ,output-state-var))))
         (error "No applicable semantics for production: ~a" ,production)))))
      
(defun execute-program (semantics node input-state)
  (let (result abort-exit)
    (tagbody
       (let ((*program-execution-exit-hook* #'(lambda () (go abort-execution))))
         (setf result (%execute-program semantics node input-state)))
       (go finish-execution)
     abort-execution
       (setf abort-exit t)
     finish-execution)
    (if abort-exit
        (progn
          ;(format *trace-output* "; ABORT EXIT: ~a~%" node)
          (values nil nil))
        (values result t))))

(defun %execute-program (semantics node input-state)
  (dolist (sem (operational-semantics-for-production semantics (production node) node))
    (let ((child-semantics (map 'list
                                #'(lambda (c)
                                    (lambda (is) (funcall #'%execute-program semantics c is)))
                                (children node))))
      (multiple-value-bind (result valid) (apply sem input-state child-semantics)
        (when (or (not (null result)) valid)
          (return-from %execute-program result)))))
  (error "No applicable semantics: ~a" (g:name (operator node))))

(defun subst-application (relation old-fn-name new-name-computer when arg-transformer)
  (when (typep relation 'smt::expression)
    (when (and (string= (smt:name relation) old-fn-name)
               (apply when (smt:children relation)))
      (setf (slot-value relation 'smt:name) (apply new-name-computer (smt:children relation)))
      (setf (slot-value relation 'smt:children) (apply arg-transformer (smt:children relation)))
      (setf (slot-value relation 'smt:child-sorts) (map 'list #'smt:sort (smt:children relation)))
      (setf (slot-value relation 'smt:arity) (length (smt:children relation))))
    (setf (slot-value relation 'smt:children)
          (map 'list #'(lambda (c) (subst-application c old-fn-name new-name-computer when arg-transformer))
               (smt:children relation))))
  relation)

(defun as-smt-query (node semantics rel-name)
  ;; Convert children
  (let (smt child-names rel)
    (dolist (c (children node))
      (let ((n (symbol-name (gensym "node"))))
        (push n child-names)
        (setf smt (append smt (as-smt-query c semantics n)))))

    (setf child-names (nreverse child-names))
    ;; Grab our semantics TODO - handle multiple semantics per production
    (setf rel (first (relational-semantics-for-production semantics (production node))))
        
    ;; Substitute relations
    (dolist (d (map 'list #'(lambda (c) (relational-semantics-for-non-terminal semantics (g:instance (production c)))) (children node)))
      (subst-application (smt:definition rel)
                         (smt:name d)
                         #'(lambda (ix &rest args)
                             (declare (ignore args))
                             (nth (1- ix) child-names))
                         (constantly t)
                         #'(lambda (ix &rest args)
                             (declare (ignore ix))
                             args)))

    ;; Update our name
    (subst-application (smt:definition rel)
                       (smt:name rel)
                       (constantly rel-name)
                       (constantly t)
                       #'(lambda (&rest args) args))
    (setf (slot-value rel 'smt:name) rel-name)

    ;; Return SMT forms
    (append smt (list (smt:to-smt rel)))))
    
