;;;;
;;;; Program atoms - either a node or a hole
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

(defclass program-atom ()
  ((compile-cache :accessor %compile-cache
                  :initarg :compile-cache
                  :documentation "A-list of descriptors to compiled functions")
   (compilation-flag :accessor %compilation-flag
                     :initarg :compilation-flag
                     :documentation "T if currently compiling this node, else NIL"))
  ;;(:default-initargs :compile-cache nil :compilation-flag nil)
  (:documentation "The top-level AST node or hole class"))

;;;
;;; Cache handling
;;;
(defun check-compile-cache (atom descriptor)
  "Checks the compile cache for ATOM with descriptor DESCRIPTOR. Returns NIL if nothing
cached, or a transformer function taking and returning states"
  (and (slot-boundp atom 'compile-cache)
       (*:when-let (cell (assoc descriptor (%compile-cache atom)))
         (cdr cell))))

(defun compile-or-cache (atom descriptor comp-fn)
  "Compiles ATOM if needed, else returns value from the cache"
  (*:if-let (fn (and (slot-boundp atom 'compile-cache)
                     (*:assocdr descriptor (%compile-cache atom))))
    fn
    (if (and (slot-boundp atom 'compilation-flag)
             (%compilation-flag atom))
        #'(lambda (is) ; Don't recursively compile nodes - return a cache lookup lambda
            (declare (type smt:state is))
            (declare (optimize (speed 3)))
            (let ((fn (*:assocdr descriptor (%compile-cache atom) :test #'eql)))
              (declare (type (function (smt:state) smt:state) fn))
              (cond
                ((> *self-recursion-counter* *self-recursion-limit*)
                 (abort-program-execution)))
                ;; TODO: can we safely detect when we're not making loop progress?
              (incf *self-recursion-counter*)
              (multiple-value-prog1
                  (funcall fn is)
                (decf *self-recursion-counter*))))
        (progn
          (setf (%compilation-flag atom) t)
          (let ((fn (funcall comp-fn)))
            (if (slot-boundp atom 'compile-cache)
                (push (cons descriptor fn) (%compile-cache atom))
                (setf (%compile-cache atom) (list (cons descriptor fn))))
            (setf (%compilation-flag atom) nil)
            fn)))))

(defmacro do-compile-or-cache ((atom descriptor) &body body)
  (*:with-thunk (body)
    `(compile-or-cache ,atom ,descriptor ,body)))

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

(defgeneric non-terminal (program)
  (:documentation "Returns the root non-terminal of PROGRAM"))

(defgeneric invalidate-cache (program)
  (:documentation "Invalidates caches on PROGRAM")
  (:method ((atom program-atom))
            (setf (%compile-cache atom) nil)
            (setf (%compilation-flag atom) nil)))

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
