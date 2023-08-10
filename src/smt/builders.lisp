;;;;
;;;; Builders for common SMT expressions
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defmacro $int (name)
  `(int-variable ,(maybe-quote name)))

(defmacro $bool (name)
  `(bool-variable ,(maybe-quote name)))

(defmacro $string (name)
  `(string-variable ,(maybe-quote name)))

(defun $literal (value sort)
  (make-instance 'literal
                 :value value
                 :sort sort
                 :name "<literal>"))

(defmacro $function (name (&rest arg-sorts) ret-sort &optional args expression)
  (if (cl:not (null expression))
      `(function-declaration ,(maybe-quote name)
                             (list ,@arg-sorts)
                             ,ret-sort
                             (list ,@args)
                             ,expression)
      `(function-declaration ,(maybe-quote name)
                             (list ,@arg-sorts)
                             ,ret-sort)))

(defun $apply (fn &rest args)
  (make-instance 'expression
                 :name (name fn)
                 :sort (return-sort fn)
                 :arity (arity fn)
                 :children args
                 :child-sorts (argument-sorts fn)))

(defmacro $forall ((&rest bindings) expression)
  `(quantifier-expression "forall"
                          (map 'list #'identity (list ,@bindings))
                          (map 'list #'sort (list ,@bindings))
                          ,expression))

(defmacro $exists ((&rest bindings) expression)
  `(quantifier-expression "exists"
                          (map 'list #'identity (list ,@bindings))
                          (map 'list #'sort (list ,@bindings))
                          ,expression))

;;
;; Expression operators
;;
(defun $+ (expr1 expr2)
  "Adds two expressions."
  (make-instance 'expression
                 :name "+"
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $- (expr1 expr2)
  "Subtracts two expressions."
  (make-instance 'expression
                 :name "-"
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $* (expr1 expr2)
  "Multiplies two expressions."
  (make-instance 'expression
                 :name "*"
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $< (expr1 expr2)
  "Compares two expressions."
  (make-instance 'expression
                 :name "<"
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $> (expr1 expr2)
  "Compares two expressions."
  (make-instance 'expression
                 :name ">"
                 :sort *int-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $= (expr1 expr2 &optional (smt *smt*))
  "Compares two expressions."
  (make-instance 'expression
                 :name (ensure-identifier "=" smt)
                 :sort *bool-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $not (expr)
  (make-instance 'expression
                 :name "not"
                 :sort *bool-sort*
                 :arity 1
                 :children (list expr)
                 :child-sorts (list (sort expr))))

(defun $and (&rest exprs)
  (make-instance 'expression
                 :name (ensure-identifier "and")
                 :sort *bool-sort*
                 :arity (length exprs)
                 :children exprs
                 :child-sorts (map 'list #'sort exprs)))

(defun $or (&rest exprs)
  (make-instance 'expression
                 :name (ensure-identifier "or")
                 :sort *bool-sort*
                 :arity (length exprs)
                 :children exprs
                 :child-sorts (map 'list #'sort exprs)))

(defun $xor (expr1 expr2)
  (make-instance 'expression
                 :name "xor"
                 :sort *bool-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $implies (expr1 expr2)
  (make-instance 'expression
                 :name "=>"
                 :sort *bool-sort*
                 :arity 2
                 :children (list expr1 expr2)
                 :child-sorts (list (sort expr1) (sort expr2))))

(defun $iff (expr1 expr2)
  (and ($implies expr1 expr2) ($implies expr2 expr1)))

(defun $ite (exprb exprt expre)
  (assert (equal (name (sort exprt)) (name (sort expre))))
  (assert (equal (name (sort exprb)) (name *bool-sort*)))
  (make-instance 'expression
                 :name "ite"
                 :sort (sort exprt)
                 :arity 3
                 :children (list exprb exprt expre)
                 :child-sorts (list (sort exprb) (sort exprt) (sort expre))))

(defun $true ()
  (make-instance 'literal
                 :name "true"
                 :sort *bool-sort*
                 :value (intern "true")))

(defun $false ()
  (make-instance 'literal
                 :name "false"
                 :sort *bool-sort*
                 :value (intern "false")))
