;;;;
;;;; Version Space Algebra support for program heaps and sets
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

;;;
;;; Program heaps - a DAG of program nodes
;;;
(defclass program-heap ()
  ())

;;;
;;; Program nodes - encodes sets of programs
;;;
(defclass program-node ()
  (specification :reader node-spec :initarg :specification))

(defclass program-leaf-node (program-node)
  (leaves :reader node-leaves :initarg :leaves))

(defclass program-union-node (program-node)
  ())

(defclass program-cross-node (program-node)
  ())

;;;
;;; Program sets - pointers to nodes in the program set
;;;
(defclass program-set ()
  ())
