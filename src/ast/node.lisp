;;;;
;;;; Program nodes
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

(defclass program-node (program-atom)
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
    :documentation "Children of this AST node.")))

(defmethod initialize-instance :after ((n program-node) &key)
  ;; Must have either operator or production set
  (assert (or (slot-boundp n 'operator) (slot-boundp n 'production)))
  (when (not (slot-boundp n 'operator))
    (setf (slot-value n 'operator) (g:operator (slot-value n 'production))))
  (if (not (slot-boundp n 'production))
      (setf (slot-value n 'production) nil)
      (assert (eql (slot-value n 'operator)
                   (g:operator (slot-value n 'production)))))
  (assert (= (g:arity (slot-value n 'operator))
             (length (slot-value n 'children)))))


(defmethod nth-child (n (thing program-node))
  (nth n (children thing)))

(defmethod (setf nth-child) (value n (thing program-node))
  (setf (nth n (children thing)) value))

;;;
;;; Helpers
;;;
(defmethod copy-program ((program program-node))
  "Returns a copy of the given program node."
  (make-instance 'program-node :production (production program)
                               :operator (operator program)
                               :children (map 'list
                                              #'copy-program
                                              (children program))))

(defmethod program-size ((node program-node))
  "Computes the size of NODE (number of nodes in the tree)"
  (1+ (reduce #'+ (map 'list #'program-size (children node)))))

(defmethod has-hole? ((node program-node))
  "Checks if any children of this node has holes."
  (some #'has-hole? (children node)))

(defmethod hole-count ((node program-node))
  "Counts the holes in each child of this node"
  (reduce #'+ (children node) :key #'hole-count))

;;;
;;; Printing
;;;
(defmethod print-program-operator (op children stream)
  "Prints an operator in the standard way, as a name and parenthesized children."
  (format stream "~a" (smt:identifier-string (g:name op)))
  (unless (null children)
    (format stream "(~{~a~^,~})" children)))

(defmethod print-program-node (n stream)
  "Prints a program node in the standard way, as just printing the operator."
  (print-program-operator (operator n) (children n) stream))

(defmethod print-program-node-as-smt (n stream)
  "Prints a program node N in SMT format"
  (if (null (children n))
      (format stream "~a" (smt:identifier-string (g:name (operator n))))
      (flet ((print-helper (n)
               (format stream " ")
               (print-program-node-as-smt n stream)))
        (format stream "(~a" (smt:identifier-string (g:name (operator n))))
        (map nil #'print-helper (children n))
        (format stream ")"))))
