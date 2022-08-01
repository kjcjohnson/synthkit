;;;;
;;;; Functionality for SMT context
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defvar *smt* nil "The global SMT context")

(defclass smt-context ()
  ((identifier-forward :accessor identifier-forward
                       :initarg :identifier-forward
                       :documentation "Maps identifier forms to symbols")
   (identifier-reverse :accessor identifier-reverse
                       :initarg :identifier-reverse
                       :documentation "Maps symbols to identifiers")
   (sort-forward :accessor sort-forward
                 :initarg :sort-forward
                 :documentation "Maps symbols to sorts"))
  (:default-initargs
   :identifier-forward (make-hash-table :test 'equal)
   :identifier-reverse (make-hash-table :test 'eql)
   :sort-forward (make-hash-table :test 'eql)))

(defun init-smt (&optional re-initialize)
  "Initializes the default SMT context"
  (when (cl:or re-initialize (null *smt*))
    (setf *smt* (make-instance 'smt-context))
    (add-default-sorts *smt*)))

(defun add-default-sorts (ctx)
  "Adds default sorts to an SMT context"
  (setf (gethash (ensure-identifier "Int" ctx) (sort-forward ctx))
        *int-sort*)
  (setf (gethash (ensure-identifier "Bool" ctx) (sort-forward ctx))
        *bool-sort*)
  (setf (gethash (ensure-identifier "String" ctx) (sort-forward ctx))
        *string-sort*))

;;;
;;; Handling identifiers
;;;
(defun ensure-identifier (identifier-form &optional (smt *smt*))
  "Returns an identifier symbol for the given identifier form (or symbol)"
  (when (stringp identifier-form)
    (setf identifier-form (list identifier-form)))
  (cond
    ((symbolp identifier-form)
     identifier-form)
    ((consp identifier-form)
     (let ((symb (gethash identifier-form (identifier-forward smt))))
       (when (null symb)
         (setf symb (gensym (format nil "SMT-ID-~a-" identifier-form)))
         (setf (gethash identifier-form (identifier-forward smt)) symb)
         (setf (gethash symb (identifier-reverse smt)) identifier-form))
       symb))
    (t
     (error "Not an SMT identifier form: ~a" identifier-form))))

(defun ensure-sort (name &optional (smt *smt*))
  "Returns a sort object for a given name"
  (setf name (ensure-identifier name smt))
  (let ((sort (gethash name (sort-forward smt))))
    (when (null sort)
      (setf sort (make-instance 'sort :name (gethash name
                                                     (identifier-reverse smt))))
      (setf (gethash name (sort-forward smt)) sort))
    sort))

(defun identifier-string (identifier &optional (smt *smt*))
  "Gets a (human-readable) name for an identifier, or just what was passed in"
  (let ((thing (gethash identifier (identifier-reverse smt))))
    (cond
      ((null thing)
       identifier)
      ((atom thing)
       thing)
      ((cl:= 1 (length thing))
       (first thing))
      ((cl:and (typep (first thing) 'string) (string= (first thing) "_Sy" :end1 3))
       (princ-to-string (second thing)))
      (t
       (princ-to-string thing)))))
