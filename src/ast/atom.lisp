;;;;
;;;; Program atoms - either a node or a hole
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

(defclass program-atom () ())

;;;
;;; Program atom protocols
;;;
(defgeneric print-program-operator (op children stream)
  (:documentation
   "Prints an operator OP and list of children CHILDREN to STREAM."))

(defgeneric print-program-node (n stream)
  (:documentation
   "Prints a program node N to STREAM."))

(defgeneric print-program-node-as-smt (n stream)
  (:documentation "Prints a program node N to STREAM in SMT format"))

(defgeneric nth-child (n thing)
  (:documentation "Retrieves the nth child of the thing"))

(defgeneric (setf nth-child) (value n thing)
  (:documentation "Sets the nth child of the thing"))

(defgeneric copy-program (program)
  (:documentation "Copies the given PROGRAM and all sub-nodes."))

(defgeneric program-size (program)
  (:documentation "Computes the size of PROGRAM (number of nodes in the tree)."))

(defgeneric has-hole? (program)
  (:documentation "Checks if the given program has any holes"))

(defgeneric hole-count (program)
  (:documentation "Counts the number of holes in PROGRAM"))

(defmacro swap-nth-child (place n thing)
  "Swaps the Nth child of THING with PLACE."
  `(psetf ,place (nth-child ,n ,thing)
          (nth-child ,n ,thing) ,place))

(defvar *printing-program-node* nil
  "Whether or not printing a program node is currently in progress.")

(defun ast-node-print-object-helper (obj stream)
  (if *printing-program-node*
      (print-program-node obj stream)
      (let ((*printing-program-node* t))
        (print-unreadable-object (obj stream :type t)
          (print-program-node obj stream)))))

(defmethod print-object ((n program-atom) stream)
  (ast-node-print-object-helper n stream))

(defun print-program (program stream)
  "Prints a program in a human-interpretable format"
  (declare (type program-atom program))
  (let ((*printing-program-node* t))
    (uiop:with-output (stream)
      (print-program-node program stream))))

(defmacro break-on-program (program string)
  "Breaks if PROGRAM prints to STRING."
  `(when (string= (print-program ,program nil) ,string)
     (break)))
