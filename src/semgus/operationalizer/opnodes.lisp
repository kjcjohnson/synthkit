;;;;
;;;;  Operationalization nodes
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.operationalizer)

(defclass op-node ()
  ((next :accessor node-next
         :initarg :next
         :type (or op-node null)
         :documentation "The next node"))
  (:default-initargs :next nil)
  (:documentation "Top level node type for operationalizations"))

(defclass begin-node (op-node)
  ((name :reader node-name
         :initarg :name
         :documentation "Name for this op graph")
   (terms :reader node-terms
          :initarg :terms
          :documentation "Child terms for this op graph"))
  (:documentation "Operationalization start"))

(defclass end-node (op-node)
  ()
  (:documentation "Operationalization end"))

(defclass block-node (op-node)
  ((inputs :reader node-inputs
           :initarg :inputs
           :type list
           :documentation "Input symbols for this node")
   (outputs :reader node-outputs
            :initarg :outputs
            :type list
            :documentation "Output symbols for this node"))
  (:default-initargs :inputs nil :outputs nil)
  (:documentation "An executable block with inputs and outputs"))
  
(defclass input-node (block-node)
  ()
  (:documentation "The input node"))

(defclass output-node (block-node)
  ()
  (:documentation "The output node"))

(defclass disjunction-node (block-node)
  ((disjuncts :reader node-disjuncts
              :initarg :disjuncts
              :documentation "Disjunction sub-nodes"))
  (:documentation "A list of disjunctions to run conditionally"))

(defclass child-node (block-node)
  ((descriptor :reader node-descriptor
               :initarg :descriptor
               :documentation "Descriptor for child semantics call")
   (term :reader node-term
         :initarg :term
         :documentation "Term symbol for child semantics call"))
  (:documentation "A call to child semantics"))

(defclass smt-node (block-node)
  ((smt :reader node-smt
        :initarg :smt
        :documentation "The SMT of the node"))
  (:documentation "A block backed by an SMT expression"))

(defclass assignment-node (smt-node)
  ()
  (:documentation "An assignment to a variable"))

(defclass guard-node (smt-node)
  ()
  (:documentation "A Boolean-valued guard expression"))

;;;
;;; Regular printing
;;;
(defmethod print-object ((node begin-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~a;~_ terms: ~a"
            (smt:identifier-string (node-name node))
            (mapcar #'smt:identifier-string (node-terms node)))))

(defmethod print-object ((node end-node) stream)
  (print-unreadable-object (node stream :type t)))

(defmethod print-object ((node input-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "inputs:~_ ~a" (mapcar #'smt:identifier-string
                                        (node-outputs node)))))

(defmethod print-object ((node output-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "outputs:~_ ~a" (mapcar #'smt:identifier-string
                                         (node-inputs node)))))

(defmethod print-object ((node child-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~@<desc: ~a; ~_term: ~a; ~_in: ~a; ~_out: ~a~:>"
            (smt:identifier-string (node-descriptor node))
            (smt:identifier-string (node-term node))
            (mapcar #'smt:identifier-string (node-inputs node))
            (mapcar #'smt:identifier-string (node-outputs node)))))

(defmethod print-object ((node guard-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~@<smt: ~a; ~_in: ~a~:>"
            (smt:to-smt (node-smt node) :pprint t)
            (mapcar #'smt:identifier-string (node-inputs node)))))

(defmethod print-object ((node smt-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~@<smt: ~a; ~_in: ~a ~_out: ~a~:>"
            (smt:to-smt (node-smt node) :pprint t)
            (mapcar #'smt:identifier-string (node-inputs node))
            (mapcar #'smt:identifier-string (node-outputs node)))))

(defun print-op-graph (stream node)
  "Prints an op node graph"
  (format stream "=====================~%")
  (loop for n = node then (node-next n)
        until (null n)
        do (format stream "~a~%" n)
        unless (null (node-next n))
          do (format stream "     ↓      ~%")))

;;;
;;; Pretty Printing
;;;
(defgeneric pprint-opnode (stream node)
  (:documentation "Pretty-prints an op node to STREAM"))

(defmethod pprint-opnode :around (stream (node op-node))
  "Leading header and footer for an op node"
  (let ((node-name (string (class-name (class-of node)))))
    (format stream "╒═══════════╡~a╞═══════════~%" node-name)
    (call-next-method)
    (format stream "╘~:[╤~;═~]═══════════~a════════════"
            (null (node-next node))
            (make-string (length node-name) :initial-element #\═))))

(defmethod pprint-opnode (stream (node op-node)) nil)

(defmethod pprint-opnode (stream (node begin-node))
  (format stream "~@<│ ~@;~a;~_ terms: ~a~:>~%"
          (smt:identifier-string (node-name node))
          (mapcar #'smt:identifier-string (node-terms node))))

(defmethod pprint-opnode (stream (node end-node)))

(defmethod pprint-opnode (stream (node input-node))
  (format stream "~@<│ ~@;inputs:~_ ~a~:>~%" (mapcar #'smt:identifier-string
                                                     (node-outputs node))))

(defmethod pprint-opnode (stream (node output-node))
  (format stream "~@<│ ~@;outputs:~_ ~a~:>~%" (mapcar #'smt:identifier-string
                                                      (node-inputs node))))

(defmethod pprint-opnode (stream (node child-node))
  (format stream "~@<│ ~@;~a[~a]; ~_in: ~a; ~_out: ~a~:>~%"
          (smt:identifier-string (node-descriptor node))
          (smt:identifier-string (node-term node))
          (mapcar #'smt:identifier-string (node-inputs node))
          (mapcar #'smt:identifier-string (node-outputs node))))

(defmethod pprint-opnode (stream (node smt-node))
  (format stream "~@<│ ~@;smt: ~a; ~_in: ~a ~_out: ~a~:>~%"
          (smt:to-smt (node-smt node) :pprint t)
          (mapcar #'smt:identifier-string (node-inputs node))
          (mapcar #'smt:identifier-string (node-outputs node))))

(defmethod pprint-opnode (stream (node assignment-node))
  (format stream "~@<│ ~@;~a ← ~a; ~_in: ~a~:>~%"
          (smt:identifier-string (first (node-outputs node)))
          (smt:to-smt (node-smt node) :pprint t)
          (mapcar #'smt:identifier-string (node-inputs node))))

(defmethod pprint-opnode (stream (node disjunction-node))
  (format stream "~@<│ ~@;in: ~a ~_out: ~a~:>~%"
          (mapcar #'smt:identifier-string (node-inputs node))
          (mapcar #'smt:identifier-string (node-outputs node)))
  (pprint-logical-block (stream (node-disjuncts node))
    (pprint-exit-if-list-exhausted)
    (loop for next = (pprint-pop)
          do
             (format stream "├────~@:_")
             (pprint-logical-block (stream nil :per-line-prefix "│ ")
               (pprint-opgraph stream next :recursive t))
             (fresh-line stream)
             (pprint-exit-if-list-exhausted))))

(defun pprint-opgraph (stream opgraph &key recursive)
  (loop for node = opgraph then (node-next node)
        until (null node)
        do (pprint-opnode stream node)
        unless (null (node-next node))
          do (format stream "~& │~% ▼~%"))
  (unless recursive
    (terpri stream)))

(set-pprint-dispatch 'op-node #'pprint-opgraph)
