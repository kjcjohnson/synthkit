;;;;
;;;; An operationalization of a CHC
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.operationalizer)

;;; Node types?

(defclass operationalization ()
  ((data-flow-graph :accessor data-flow-graph)
   (control-flow-graph :accessor control-flow-graph)
   (attributes :accessor attributes
               :initarg :attributes
               :type hash-table
               :documentation "Attributes associated with this operationalization"))
  (:documentation "An operationalization of a CHC; that is, a re-encoding of the CHC's
semantics in a format that can be operationally executed."))

;;; Slit CHCs into nodes here
;;; Then create the data flow graph? And then the control flow graph?

;;; Mayhaps the data flow graph should start out being undirected
;;;  and then we can find directions as a part of the operationalizer
;;; (I think we do this already, but may be better to do it more formally)
