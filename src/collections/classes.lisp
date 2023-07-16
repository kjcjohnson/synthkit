;;;;
;;;; Abstract collection classes
;;;;
(in-package #:com.kjcjohnson.synthkit.collections)

(defclass collection () ()
  (:documentation "A generic collection type"))

(defclass set (collection) ()
  (:documentation "A generic set type"))

(defclass map (collection) ()
  (:documentation "A generic map between keys and values"))

;;;
;;; Key-value pairs
;;;
(defstruct (kvp (:constructor kvp (key value))
                (:conc-name))
  key value)
