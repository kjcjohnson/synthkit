;;;;
;;;; CHCs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.chc)

(defclass chc ()
  ((symbols :reader symbol-table
            :initarg :symbols
            :type symbol-table
            :documentation "Symbols for this CHC")))

