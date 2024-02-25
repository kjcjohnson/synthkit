;;;;
;;;; The SemGuS Context - information about the problem being solved
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defvar *semgus-context* nil "Information about problem being parsed")

(defclass semgus-context ()
  ((grammar :accessor grammar
            :initarg :grammar)
   (chcs :accessor chcs
         :initarg :chcs)
   (head-relations :accessor head-relations
                   :initarg :head-relations)
   (constraints :accessor constraints
                :initarg :constraints)
   (root-relations :accessor root-relations
                   :initarg :root-relations)
   (term-name :accessor term-name
              :initarg :term-name)
   (term-type :accessor term-type
              :initarg :term-type)
   (auxiliary-functions :accessor auxiliary-functions
                        :initarg :auxiliary-functions)
   (metadata
    :initarg :metadata
    :reader metadata
    :type hash-table
    :documentation "Arbitrary key-value metadata")
   (path
    :initarg :path
    :reader path)
   (term-types :accessor term-types
               :initarg :term-types
               :type list ; of SORTs
               :documentation "Term types for this problem"))
  (:default-initargs
   :chcs nil
   :head-relations nil
   :constraints nil
   :auxiliary-functions nil
   :metadata (make-hash-table)
   :path nil
   :term-types nil))

(defun lookup-head (name &optional (context *semgus-context*))
  "Looks up a CHC head with the given name"
  (setf name (smt:ensure-identifier name))
  (find name (head-relations context) :key #'chc:name))

(defun add-head (head &optional (context *semgus-context*))
  "Adds a CHC head to the context"
  (when (lookup-head (chc:name head) context)
    (error "Attempt to add a duplicate head with the name: ~a" (chc:name head)))
  (push head (head-relations context)))

(defun lookup-root (name &optional (context *semgus-context*))
  "Looks up a CHC root with the given name"
  (find name (root-relations context) :key #'chc:name))

(defun lookup-chc (id &optional (context *semgus-context*))
  "Looks up a CHC in CONTEXT with the given identifier ID"
  (setf id (smt:ensure-identifier id))
  (find id (chcs context) :key #'chc:id))

(defun lookup-chcs-by-operator (operator &optional (context *semgus-context*))
  "Looks up all CHCs in CONTEXT with a constructor matching OPERATOR"
  (let ((name (smt:ensure-identifier (g:name operator))))
    (remove-if-not (a:curry #'eql name)
                   (chcs context)
                   :key (a:compose #'chc:name #'chc:constructor))))

;;;
;;; Grammar helpers
;;;
(defmethod g:lookup-non-terminal ((context semgus-context) name)
  (let ((nt (g:lookup-non-terminal (grammar context) (smt:ensure-identifier name))))
    (if (null nt) ;; Sometimes the term type name gets used...as a string
        (find name (g:non-terminals (grammar context))
              :test #'string=
              :key #'(lambda (nt) (first (smt:name (g:term-type nt)))))
        nt)))

(defmethod g:lookup-operator ((context semgus-context) name)
  (g:lookup-operator (grammar context) (smt:ensure-identifier name)))

(defmethod g:lookup-production ((context semgus-context) name)
  (g:lookup-production (grammar context) (smt:ensure-identifier name)))

(defmethod g:non-terminals-for-term-type ((context semgus-context) term-type)
  (g:non-terminals-for-term-type (grammar context) term-type))

(defmethod g:non-terminals ((context semgus-context))
  (g:non-terminals (grammar context)))

(defmethod g:productions ((context semgus-context))
  (g:productions (grammar context)))

;;;
;;; Term types (n.b.: these aren't actually used for synthesis, but we need them
;;;  to export problem data to other systems. So we save them for bookkeeping.)
;;;
(defclass term-type (smt:sort)
  ((constructors :reader term-type-constructors
                 :initarg :constructors
                 :type list
                 :documentation "A list of term type constructors for this term type"))
  (:default-initargs :constructors nil)
  (:documentation "A term type"))

(defclass term-type-constructor ()
  ((parent :reader parent-term-type
           :initarg :parent
           :type term-type
           :documentation "The LHS of this term type constructor")
   (operator :reader operator
             :initarg :operator
             :documentation "The operator symbol of this constructor")
   (children :reader children
             :initarg :children
             :type list
             :documentation "The children of this constructor"))
  (:default-initargs :children nil)
  (:documentation "A constructor (i.e., production) for a term type"))

(defun add-term-type-constructor (term-type operator children)
  "Adds a new constructor to TERM-TYPE with OPERATOR and CHILDREN"
  (declare (type term-type term-type))
  (push (make-instance 'term-type-constructor :parent term-type
                                              :operator operator
                                              :children children)
        (slot-value term-type 'constructors)))
