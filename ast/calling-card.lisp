;;;;
;;;; Calling Cards - how to call semantic builder functions
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

;;;
;;; Calling Card protocol definition
;;;
(defgeneric semantics-descriptor-requests (calling-card)
  (:documentation "Gets the semantics descriptor requests for this card."))

(defgeneric semantic-builder-function (calling-card)
  (:documentation "Gets the semantic builder function for this card."))

(defgeneric semantics-descriptor-request-descriptor (descriptor-request)
  (:documentation "Gets the descriptor associated with the given request"))

(defgeneric semantics-descriptor-request-node-id (descriptor-request)
  (:documentation "Gets the node ID associated with the given request"))

;;;
;;; Default implementations
;;;
(defclass calling-card ()
  ((descriptor-requests :initarg :descriptor-requests
                        :reader semantics-descriptor-requests
                        :documentation "List of descriptor requests")
   (builder-function :initarg :builder-function
                     :reader semantic-builder-function
                     :documentation "Function for building semantic functions")))

(defclass semantics-descriptor-request ()
  ((descriptor :initarg :descriptor
               :reader semantics-descriptor-request-descriptor
               :documentation "The semantics descriptor for this request")
   (node-id :initarg :node-id
            :reader semantics-descriptor-request-node-id
            :documentation "The node-id, either NIL, :SELF, or an integer index"))
  (:default-initargs :node-id nil)
  (:documentation "A descriptor request for calling cards"))

(defmethod print-object ((request semantics-descriptor-request) stream)
  "Prints a descriptor request to the stream"
  (print-unreadable-object (request stream :type t)
    (format stream
            "~a [~a]"
            (semantics-descriptor-request-descriptor request)
            (semantics-descriptor-request-node-id request))))
