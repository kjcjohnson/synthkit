;;;
;;; Semantics.lisp - semantics helpers
;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defclass default-semantics ()
  ((operational-semantics
    :initarg :operational
    :reader operational-semantics)
   (relational-semantics
    :initarg :relational
    :reader relational-semantics)
   (relation-definitions
    :initarg :relation-definitions
    :reader relation-definitions)))

(defmethod ast:operational-semantics-for-production ((sem default-semantics) production node)
  (funcall (operational-semantics sem) production))

(defmethod ast:relational-semantics-for-production ((sem default-semantics) production)
  (funcall (relational-semantics sem) production))

(defmethod ast:relational-semantics-for-non-terminal ((sem default-semantics) non-terminal)
  (funcall (relation-definitions sem) non-terminal))

#| Example:
(defsemantics *max2*
  :sorts ((Int *int-sort*) (Bool *bool-sort*))
  :variables ((x Int) (y Int)) 
  :operational
  ((Start (is)
     ("x" () ((cdr (assoc 'x is))))
     ...
     ("+" (sem1 sem2)
        ((+ (funcall sem1 is) (funcall sem2 is)))))
   (B (is) ...etc.))

  :relational
  ((Start (s.sem Int Int Int Bool)
     ("x" () 
|#


(defun expand-variable-declaration (pair)
  (destructuring-bind (var sort) pair
    (list
     var
     `(smt:variable (quote ,var) ,sort))))

(defun destructure-operational-semantics (sem)
  (let ((prod-v (gensym "PRD"))
        (name-v (gensym "NAME")))
    `(lambda (,prod-v)
       (let ((,name-v (g:name ,prod-v)))
         (cond
           ,@(loop for s in sem appending
              (destructuring-bind (nt (&rest parameters) &rest productions) s
                (declare (ignore nt))
                (loop for p in productions collecting
                  (destructuring-bind (prod (&rest children) &rest funs) p
                    `((string= ,name-v ,prod)
                      (list ,@(map 'list
                                   #'(lambda (f)
                                       `#'(lambda ,(append parameters children)
                                            ,@f))
                                   funs))))))))))))

(defun expand-sem-declaration (decl)
  (destructuring-bind (rel-name &rest types) decl
    `(,rel-name (smt:$function ,rel-name ,types smt:*bool-sort*)))) 

(defun destructure-relational-semantics (sem)
  (let ((prod-v (gensym "PRD"))
        (name-v (gensym "NAME")))
    `(lambda (,prod-v)
       (let ((,name-v (g:name ,prod-v)))
           (cond
             ,@(loop for s in sem
                     appending
                     (destructuring-bind (nt rel-decl &rest productions) s
                       (declare (ignore nt rel-decl))               
                       (loop for p in productions
                             collecting
                             (destructuring-bind (prod (&rest args) &rest rels) p
                               `((string= ,name-v ,prod)
                                 (list ,@(map 'list
                                              #'(lambda (f)
                                                  `(smt::function-declaration
                                                    ',(gensym "rel-")
                                                    (map 'list #'smt:sort (list ,@args))
                                                    smt:*bool-sort*
                                                    (list ,@args)
                                                    ,f))
                                              rels))))))))))))

(defmacro defsemantics (name &key sorts variables operational relational)
  "Defines semantics for a type."
  `(defparameter ,name
     (let (,@sorts)
       (let (,@(map 'list #'expand-variable-declaration variables))
         (let (,@(when relational
                   (delete-duplicates
                    (map 'list
                         #'(lambda (x)
                             (expand-sem-declaration (second x)))
                         relational)
                    :key #'first)))
             
         (make-instance 'default-semantics
                        ,@(when operational
                            `(:operational
                              ,(destructure-operational-semantics operational)))
                        ,@(when relational
                            `(:relational
                              ,(destructure-relational-semantics relational)
                              :relation-definitions
                              ,(let ((nt-var (gensym "NT")))
                                 `#'(lambda (,nt-var)
                                     (cond
                                       ,@(loop for nt-form in relational
                                              collecting
                                              `((string= (g:name ,nt-var) ,(string (first nt-form)))
                                                ,(first (second nt-form)))))))))))))))
             
