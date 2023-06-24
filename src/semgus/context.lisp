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
    :reader path))
  (:default-initargs
   :chcs nil
   :head-relations nil
   :constraints nil
   :auxiliary-functions nil
   :metadata (make-hash-table)
   :path nil))

(defun lookup-head (name &optional (context *semgus-context*))
  "Looks up a CHC head with the given name"
  (find name (head-relations context) :key #'chc:name))

(defun add-head (head &optional (context *semgus-context*))
  "Adds a CHC head to the context"
  (when (lookup-head (chc:name head) context)
    (error "Attempt to add a duplicate head with the name: ~a" (chc:name head)))
  (push head (head-relations context)))

(defun lookup-root (name &optional (context *semgus-context*))
  "Looks up a CHC root with the given name"
  (find name (root-relations context) :key #'chc:name))
