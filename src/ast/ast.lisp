;;;
;;; ast.lisp - functionality for program nodes and other ast stuff
;;;
(in-package #:com.kjcjohnson.synthkit.ast)

(defgeneric semantics-descriptors-for-non-terminal (semantics non-terminal)
  (:documentation "Gets a list of semantics descriptors valid for NON-TERMINAL."))

(defgeneric operational-semantics-for-production (semantics descriptor production)
  (:documentation "Maps from a production to a list of state transformation semantic functions."))

(defgeneric operational-semantics-for-hole (semantics descriptor non-terminal)
  (:documentation "Maps from a descriptor to semantics for a hole"))

(defgeneric relational-semantics-for-production (semantics descriptor production)
  (:documentation "Maps from a production to CHC-based relational semantics."))

(defgeneric relational-semantics-for-non-terminal (semantics non-terminal)
  (:documentation "Maps from a non-terminal to the semantics relation function declaration."))

(defgeneric canonicalize-result (semantics output)
  (:documentation "Processes the output of a program into a canonical state.")
  (:method (semantics output) output)
  (:method (semantics (output smt:state)) (smt:canonicalize-state output)))

(defun compile-program (node semantics)
  (with-slots (production) node
    (when (null production) (error "Cannot compile a program node without a production."))
    (let ((input-state-var (gensym "INPUT"))
          (output-state-var (gensym "OUTPUT")))
      `(lambda (,input-state-var) ; TODO: update for descriptors properly
         ,(dolist (s (operational-semantics-for-production semantics nil production))
            `(let ((,output-state-var (funcall ,s ,input-state-var)))
               (unless (null ,output-state-var) (return ,output-state-var))))
         (error "No applicable semantics for production: ~a" ,production)))))

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
    (setf rel (first (relational-semantics-for-production semantics rel-name (production node))))
        
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
    
