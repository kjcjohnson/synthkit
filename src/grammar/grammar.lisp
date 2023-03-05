;;;
;;; Grammar functionality
;;;
(in-package #:com.kjcjohnson.synthkit.grammar)

(defclass regular-tree-grammar ()
  ((non-terminals
    :initarg :non-terminals
    :initform (list)
    :reader non-terminals)
   (operators
    :initarg :operators
    :initform (list)
    :reader operators)
   (initial-non-terminal
    :initarg :initial
    :reader initial-non-terminal)
   (productions
    :initarg :productions
    :initform (list)
    :reader productions)
   (extra-data
    :initform (make-hash-table)
    :reader %extra-data)))

(defun extra-data (grammar key &optional default)
  "Retrieves a piece of extra data from a grammar."
  (gethash key (%extra-data grammar) default))

(defun (setf extra-data) (data grammar key)
  "Sets a piece of extra data in a grammar."
  (setf (gethash key (%extra-data grammar)) data))

(defmacro do-non-terminals ((var grammar) &body body)
  `(dolist (,var (non-terminals ,grammar))
     ,@body))

(defmethod print-object ((g regular-tree-grammar) stream)
  (print-unreadable-object (g stream :type t)
    (fresh-line stream)
    (do-non-terminals (nt g)
      (format stream "  ~a -> ~:{~a~:[~;~:*(~{~a~^, ~})~] | ~};~%"
              (name nt)
              (map 'list #'(lambda (p) (list
                                        (name (operator p))
                                        (map 'list #'name (occurrences p))))
                   (productions-for-instance g nt))))))

(defclass named-grammar-element ()
  ((name
    :initarg :name
    :initform (error "Name is required.")
    :reader name
    :documentation "Name of this grammar element.")))

(defclass non-terminal (named-grammar-element)
  ((term-type :initarg :term-type
              :accessor term-type
              :documentation "Term type associated with this non-terminal")))

(defmethod print-object ((nt non-terminal) stream)
  (print-unreadable-object (nt stream :type t)
    (format stream "~s" (name nt))))

(defclass operator (named-grammar-element)
  (
   (arity
    :initarg :arity
    :initform 0
    :reader arity
    :documentation "This operator's arity: the number of children it has.")))

(defmethod print-object ((o operator) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~a" (name o))))

(defclass production (named-grammar-element)
  ((operator
    :initarg :operator
    :initform (error "Operator is required.")
    :reader operator)
   (instance
    :initarg :instance
    :reader instance)
   (occurrences
    :initarg :occurrences
    :initform (list)
    :reader occurrences)))

(defmethod initialize-instance :after ((p production) &key)
  (with-slots (operator instance occurrences) p
    (assert (= (arity operator) (length occurrences)))
    (assert (typep operator 'operator))
    (assert (typep instance 'non-terminal))))

(defmethod arity ((p production))
  (length (occurrences p)))

(defmethod print-object ((p production) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a = ~a -> ~a(~{~a~^, ~}) " ; for no parens on arity 0, use ~:[~^~;~:*~]
            (name p)
            (name (instance p))
            (name (operator p))
            (map 'list #'name (occurrences p)))))

(defun productions-for-instance (grammar non-terminal)
  "Finds all productions in GRAMMAR with the non-terminal NON-TERMINAL on the LHS."
  (remove-if-not #'(lambda (p) (equal (name non-terminal) (name (instance p)))) (productions grammar)))

(defun make-rtg (&key non-terminals initial operators productions)
  "Creates an RTG, creating non-terminals, operators, and productions as needed.
   Non-terminals should be given as a list of string-designators, operators as a
   cons of (NAME . ARITY), and productions as (NAME LHS OP CHILDREN...)."
  (let ((nts (make-hash-table :test 'equal))
        (ops (make-hash-table :test 'equal))
        (prs (list)))
    (flet ((ensure-non-terminal (nt)
             (if (typep nt 'non-terminal)
                 (progn
                   (setf (gethash (name nt) nts) nt)
                   nt)
                 (let* ((nt-as-string (string nt))
                        (nt-as-object (gethash nt-as-string nts)))
                   (when (null nt-as-object)
                     (setf nt-as-object (make-instance 'non-terminal :name nt-as-string)
                           (gethash nt-as-string nts) nt-as-object))
                   (when (null initial)
                     (setf initial nt-as-object))
                   nt-as-object)))
           (ensure-operator (op arity)
             (let* ((op-as-string op);(string op))
                    (op-as-object (gethash op-as-string ops)))
               (if (null op-as-object)
                   (setf (gethash op-as-string ops)
                         (make-instance 'operator :name op-as-string :arity arity))
                   (if (= (arity op-as-object) arity)
                       op-as-object
                       (error "Arity mismatch: ~a defined with arity ~a, but used at: ~a"
                              op-as-string (arity op-as-object) arity))))))

      ;; First, dump in any non-terminals we're explicity given
      (map nil #'ensure-non-terminal non-terminals)

      ;; Then all explicitly-given operators
      (map nil #'(lambda (op-pair) (ensure-operator (car op-pair) (cdr op-pair))) operators)

      ;; Finally, process the productions
      (dolist (p productions)
        (let ((nt-instance (ensure-non-terminal (second p)))
              (operator (ensure-operator (third p) (- (length p) 3)))
              (children (map 'list #'ensure-non-terminal (cdddr p)))
              (p-name (first p)))
          (push (make-instance 'production :name p-name
                                           :operator operator
                                           :instance nt-instance
                                           :occurrences children)
                prs)))

      ;; Now put it all together!
      (make-instance 'regular-tree-grammar
                     :non-terminals (loop for v being the hash-values of nts collecting v)
                     :initial initial
                     :operators (loop for v being the hash-values of ops collecting v)
                     :productions prs))))
