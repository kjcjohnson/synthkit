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
    :reader children
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

(defun print-program-node (n stream)
  (format stream "~a" (g:name (operator n)))
  (unless (null (children n))
    (format stream "(~{~a~^,~})" (children n))))

(defmethod print-object ((n program-node) stream)
  (if *printing-program-node*
      (print-program-node n stream)
      (let ((*printing-program-node* t))
        (print-unreadable-object (n stream :type t)
          (print-program-node n stream)))))

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

(defgeneric semantics-for-production (semantics production)
  (:documentation "Maps from a production to a list of state transformation semantic functions."))

(defun compile-program (node semantics)
  (with-slots (production) node
    (when (null production) (error "Cannot compile a program node without a production."))
    (let ((input-state-var (gensym "INPUT"))
          (output-state-var (gensym "OUTPUT")))
      `(lambda (,input-state-var)
         ,(dolist (s (semantics-for-production semantics production))
            `(let ((,output-state-var (funcall ,s ,input-state-var)))
               (unless (null ,output-state-var) (return ,output-state-var))))
         (error "No applicable semantics for production: ~a" ,production)))))
      

(defun execute-program (semantics node input-state)
  (dolist (sem (semantics-for-production semantics (production node)))
    (let ((child-semantics (map 'list
                                #'(lambda (c)
                                    (lambda (is) (funcall #'execute-program semantics c is)))
                                (children node))))
      (multiple-value-bind (result valid) (apply sem input-state child-semantics)
        (when (or (not (null result)) valid)
          (return-from execute-program result)))))
  (error "No applicable semantics: ~a" (g:name (operator node))))
