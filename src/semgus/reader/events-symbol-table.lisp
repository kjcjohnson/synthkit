;;;;
;;;; Symbols and symbol tables
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

;;
;; Symbol tables
;;
(defun com.kjcjohnson.synthkit.semgus.reader.user::symbol-entry
    (name &key sort index)
  "A symbol entry"
  (make-instance 'chc:symbol-entry :name name :sort sort :index index))

(defun com.kjcjohnson.synthkit.semgus.reader.user::symbol-table
    (&key inputs outputs term auxiliary children)
  "A CHC's symbol table"
  (make-instance 'chc:symbol-table
                 :inputs inputs
                 :outputs outputs
                 :term term
                 :auxiliary auxiliary
                 :children children))
