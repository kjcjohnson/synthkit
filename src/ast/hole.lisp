;;;;
;;;; Holes
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

(defclass program-hole (program-atom)
  ((non-terminal
    :initarg :non-terminal
    :reader non-terminal
    :documentation "Non-terminal for this AST hole.")))

(defmethod initialize-instance :after ((h program-hole) &key)
  "Ensures that a program hole has an associated non-terminal."
  (assert (and (slot-boundp h 'non-terminal)
               (typep (slot-value h 'non-terminal) 'g:non-terminal))))

;;;
;;; Copying protocol
;;;
(defmethod copy-program ((hole program-hole))
  "Returns a copy of the given HOLE...or doesn't, since holes are fungible."
  hole)

;;;
;;; Measurement protocol
;;;
(defmethod program-size ((hole program-hole))
  "Returns the size of a hole - 1, since a hole is an atom..."
  1)

(defmethod has-hole? ((hole program-hole))
  "Checks if this hole has a hole...so, yes."
  t)

(defmethod hole-count ((hole program-hole))
  "Gets the count of holes in this program...1"
  1)

;;;
;;; Printing protocol
;;;
(defmethod print-program-node ((h program-hole) stream)
  (format stream "??_~a" (g:name (non-terminal h))))
