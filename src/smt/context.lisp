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
                 :documentation "Maps symbols to sorts")
   (function-definitions :accessor function-definitions
                         :initarg :function-definitions
                         :documentation "Maps function names (ids) to functions"))
  (:default-initargs
   :identifier-forward (make-hash-table :test 'equal)
   :identifier-reverse (make-hash-table :test 'eql)
   :sort-forward (make-hash-table :test 'eql)
   :function-definitions (make-hash-table :test 'eql)))

(defun init-smt (&optional re-initialize)
  "Initializes the default SMT context"
  (when (cl:or re-initialize (null *smt*))
    (setf *smt* (make-instance 'smt-context))
    (add-default-sorts *smt*)
    (map-built-in-definitions
     #'(lambda (name fn)
         (declare (ignore fn))
         (set-function-definition name :built-in *smt*)))))

(defun add-default-sorts (ctx)
  "Adds default sorts to an SMT context"
  (setf (gethash (ensure-identifier "Int" ctx) (sort-forward ctx))
        *int-sort*)
  (setf (gethash (ensure-identifier "Bool" ctx) (sort-forward ctx))
        *bool-sort*)
  (setf (gethash (ensure-identifier "String" ctx) (sort-forward ctx))
        *string-sort*)
  (setf (gethash (ensure-identifier "RegLan" ctx) (sort-forward ctx))
        *reglan-sort*))

(defun get-sort (sort-id &optional (ctx *smt*))
  "Gets a sort by ID"
  (gethash (ensure-identifier sort-id ctx) (sort-forward ctx)))

(defun (setf get-sort) (sort sort-id &optional (ctx *smt*))
  "Sets a sort by ID"
  (setf (gethash (ensure-identifier sort-id ctx) (sort-forward ctx)) sort))

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
      ((cl:and (typep (first thing) 'string)
               (>= (length (first thing)) 3)
               (string= (first thing) "_Sy" :end1 3))
       (princ-to-string (second thing)))
      (t
       (princ-to-string thing)))))

(defun identifier-smt (identifier &optional (smt *smt*))
  "Gets the SMT name for the given identifier"
  (unless (symbolp identifier)
    (setf identifier (ensure-identifier identifier smt)))
  (let ((thing (gethash identifier (identifier-reverse smt))))
    (if (= 1 (length thing))
        (first thing)
        thing))) ; TODO: probably broken for indexed identifiers

(defparameter *unique-id-counter* 0 "Counter for unique identifiers")

(defun unique-identifier (&optional (smt *smt*))
  "Returns a unique SMT identifier"
  (loop for temp-id = (list (format nil "__U-~a" (incf *unique-id-counter*)))
        until (null (gethash temp-id (identifier-forward smt)))
        finally (return (ensure-identifier temp-id smt))))

;;;
;;; Functions and their definitions
;;;
(defclass function-definition ()
  ((name :accessor name
         :initarg :name
         :documentation "This function's name")
   (definition :accessor definition
               :initarg :definition
               :documentation "This function as an SMT expression (or :built-in)")
   (compiled-definition :accessor %compiled-definition
                        :initarg :compiled-definition
                        :documentation "Function object implementing function")))

(defmethod print-object ((fn-defn function-definition) stream)
  "Prints a function definition object with the function name"
  (print-unreadable-object (fn-defn stream :type t :identity t)
    (format stream "~a" (name fn-defn))))

(defun is-built-in? (defn)
  "Checks if a function definition is a built-in definition"
  (eql :built-in (definition defn)))

(defun get-function-definition (fn-name &optional (context *smt*))
  "Gets a function definition object for the given name"
  (let* ((id-name (ensure-identifier fn-name context))
         (result (gethash id-name (function-definitions context))))
    ;; It's possible FN-NAME refers to an abstract not-yet-instantiated indexed fn
    (if (null result)
        (let* ((smt-name (gethash id-name (identifier-reverse context)))
               (abs (indexed-base-name smt-name :concrete nil)))
          (unless (null abs)
            (set-function-definition id-name :built-in context)))
        result)))

(defun set-function-definition (fn-name definition &optional (context *smt*))
  "Sets a function definition object in the given context"
  (setf (gethash (ensure-identifier fn-name context)
                 (function-definitions context))
        (make-instance 'function-definition
                       :name (ensure-identifier fn-name context)
                       :definition definition
                       :compiled-definition nil)))

(defun compiled-definition (defn &optional (context *smt*))
  "Gets the compiled definition for a function, compiling if needed."
  (let ((compiled (%compiled-definition defn)))
    (when (null compiled)
      (setf compiled
            (if (is-built-in? defn)
                (lookup-theory-function (gethash (name defn)
                                                 (identifier-reverse context)))
                (progn
                  (format *trace-output*
                          "; Compiling definition: ~a~%"
                          (identifier-string (name defn)))
                  (compile-definition defn))))
      (setf (%compiled-definition defn) compiled))
    compiled))

(defun compiled-definition-form (defn &optional (context *smt*))
  "Gets a form that, when evaluated, returns the compiled definition"
  (declare (ignore context))
  `(%compiled-definition ,defn))

(defun get-compiled-function (fn-name &optional (context *smt*))
  "Gets a compiled version of the function named by FN-NAME"
  (let ((defn (get-function-definition fn-name context)))
    (unless (null defn)
      (compiled-definition defn context))))

;;;
;;; Datatype handling
;;;
(defun add-datatype-constructor (datatype constructor &optional (context *smt*))
  "Adds a datatype constructor function to the context."
  (setf (gethash (ensure-identifier (name constructor))
                 (function-definitions context))
        (make-instance 'function-definition
                       :name (ensure-identifier (name constructor))
                       :definition nil
                       :compiled-definition #'(lambda (&rest args)
                                                (make-datatype-instance datatype
                                                                        constructor
                                                                        args)))))
