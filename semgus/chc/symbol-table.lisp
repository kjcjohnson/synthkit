;;;;
;;;; Symbol tables for CHCs
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.chc)

(defclass symbol-entry ()
  ((name :reader symbol-name
         :initarg :name
         :type symbol
         :documentation "The symbol name (as an SMT symbol)")
   (sort :reader symbol-sort
         :initarg :sort
         :type symbol
         :documentation "The symbol sort (as an SMT symbol)")
   (index :reader symbol-index
          :initarg :index
          :type (or integer null)
          :documentation "The symbol index, or NIL if not indexed"))
  (:documentation "A symbol entry in the symbol table. Each entry includes the
symbol name, the symbol sort, and optionally the symbol index of where it appears
in the associated signature: either the CHC head relation or the term constructor."))

(defclass symbol-table ()
  ((inputs :reader input-symbols
           :initarg :inputs
           :type list
           :documentation "Symbols that are inputs in the CHC")
   (outputs :reader output-symbols
            :initarg :outputs
            :type list
            :documentation "Symbols that are outputs in the CHC")
   (term :reader term-symbol
         :initarg :term
         :type symbol-entry
         :documentation "The symbol for the program term in the CHC")
   (auxiliary :reader auxiliary-symbols
              :initarg :auxiliary
              :type list
              :documentation "Symbols that are bound over the CHC body")
   (children :reader child-symbols
             :initarg :children
             :type list
             :documentation "Symbols that are the child term names in the CHC"))
  (:documentation "Table of symbols for a CHC. All but TERM are lists of symbol
entries, while TERM is a single entry. INPUTS, OUTPUTS, and TERM are bound in the
outermost scope of the CHC, while CHILDREN and AUXILIARY in the body only (with
AUXILIARY bound inside the scope of CHILDREN). This means that the same symbol can
appear in these three scopes, and the innermost binding takes precedence.

For convenience, the readers for this class are also defined on CHC."))

;;;
;;; Convenience readers for CHC
;;;
(defmethod input-symbols ((chc chc)) (input-symbols (symbol-table chc)))
(defmethod output-symbols ((chc chc)) (output-symbols (symbol-table chc)))
(defmethod term-symbol ((chc chc)) (term-symbol (symbol-table chc)))
(defmethod auxiliary-symbols ((chc chc)) (auxiliary-symbols (symbol-table chc)))
(defmethod child-symbols ((chc chc)) (child-symbols (symbol-table chc)))
