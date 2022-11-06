;;;
;;; ast.lisp - functionality for program nodes and other ast stuff
;;;
(in-package #:com.kjcjohnson.synthkit.ast)

(defgeneric operational-semantics-for-production (semantics production node)
  (:documentation "Maps from a production to a list of state transformation semantic functions."))

(defgeneric relational-semantics-for-production (semantics production)
  (:documentation "Maps from a production to CHC-based relational semantics."))

(defgeneric relational-semantics-for-non-terminal (semantics non-terminal)
  (:documentation "Maps from a non-terminal to the semantics relation function declaration."))

(defgeneric canonicalize-result (semantics output)
  (:documentation "Processes the output of a program into a canonical state.")
  (:method (semantics output) output)
  (:method (semantics (output smt:state)) (smt:canonicalize-state output)))

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

(defvar *execution-counter* 0)

(defun execute-program (semantics node input-state)
  (incf *execution-counter*)
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
        (values (canonicalize-result semantics result) t))))

(defvar *self-recursion-counter* 0
  "Counts recursion depth to find probably-non-terminating programs.")
(defparameter *self-recursion-limit* 200)

(defun %execute-program (semantics node input-state)

  ;;; Short-circuit and yell if we attempt to execute a hole as a program
  (when (typep node 'program-hole)
    (error "Attempting to execute hole: ~a" node))
  
  (dolist (sem (operational-semantics-for-production semantics (production node) node))
    (let ((child-semantics (map 'list
                                #'(lambda (c)
                                    (lambda (is)
                                      (values
                                       (funcall #'%execute-program semantics c is)
                                       t)))
                                (children node)))
          (self-semantics #'(lambda (is)
                              (let ((*self-recursion-counter*
                                      (1+ *self-recursion-counter*)))
                                (if (or (> *self-recursion-counter*
                                           *self-recursion-limit*)
                                        (smt:state= input-state is))
                                    (abort-program-execution)
                                    (values
                                     (funcall #'%execute-program semantics node is)
                                     t))))))
      (multiple-value-bind (result valid)
          (apply sem input-state self-semantics child-semantics)
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
    
