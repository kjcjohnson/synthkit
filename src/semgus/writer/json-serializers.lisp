;;;;
;;;; JSON serializers for various SemGuS and SMT types
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.writer)

(defmethod jzon:write-value ((writer jzon:writer) (sort smt:sort))
  "Writes an SMT sort object"
  (if (zerop (smt:arity sort))
      (jzon:write-value writer (smt:identifier-smt (smt:name sort)))
      (jzon:with-object writer
        (jzon:write-key writer "kind")
        (jzon:write-value writer (smt:identifier-smt (smt:name sort)))
        (jzon:write-key writer "params")
        (jzon:with-array writer
          (map nil (*:curry #'jzon:write-value writer) (smt:sort-parameters sort))))))

(defmethod jzon:write-value ((writer jzon:writer) (relation chc:head))
  "Writes a CHC body relation"
  (jzon:with-object writer
    (jzon:write-key writer "name")
    (jzon:write-value writer (smt:identifier-smt (chc:name relation)))
    (jzon:write-key writer "signature")
    (jzon:write-value writer (chc:signature relation))
    (jzon:write-key writer "arguments")
    (jzon:write-value writer (map 'list #'smt:identifier-smt (chc:formals relation)))))

(defmethod jzon:write-value ((writer jzon:writer) (relation chc:relation))
  "Writes a CHC body relation"
  (jzon:with-object writer
    (jzon:write-key writer "name")
    (jzon:write-value writer (smt:identifier-smt (chc:name relation)))
    (jzon:write-key writer "signature")
    (jzon:write-value writer (chc:signature relation))
    (jzon:write-key writer "arguments")
    (jzon:write-value writer (map 'list #'smt:identifier-smt (chc:actuals relation)))))

(defmethod jzon:write-value ((writer jzon:writer) (ttc semgus:term-type-constructor))
  "Writes a term type constructor"
  (jzon:with-object writer
    (jzon:write-key writer "name")
    (jzon:write-value writer (smt:identifier-smt (semgus:operator ttc)))
    (jzon:write-key writer "children")
    (jzon:with-array writer
      (map nil (*:curry #'jzon:write-value writer) (semgus:children ttc)))))

(defmethod jzon:write-value ((writer jzon:writer) (cc chc:constructor))
  "Writes a CHC constructor"
  (jzon:with-object writer
    (jzon:write-key writer "name")
    (jzon:write-value writer (smt:identifier-smt (chc:name cc)))
    (jzon:write-key writer "arguments")
    (jzon:write-value writer (map 'vector #'smt:identifier-smt (chc:arguments cc)))
    (jzon:write-key writer "argumentSorts")
    (jzon:write-value writer (coerce (chc:argument-sorts cc) 'vector))
    (jzon:write-key writer "returnSort")
    (jzon:write-value writer (chc:return-sort cc))))

(defmethod jzon:write-value ((writer jzon:writer) (se chc:symbol-entry))
  "Writes a symbol entry"
  (jzon:with-object writer
    (jzon:write-key writer "id")
    (jzon:write-value writer (smt:identifier-smt (chc:symbol-name se)))
    (jzon:write-key writer "index")
    (jzon:write-value writer (or (chc:symbol-index se) 'null))
    (jzon:write-key writer "sort")
    (jzon:write-value writer (chc:symbol-sort se))))

(defmethod jzon:write-value ((writer jzon:writer) (rtg g:regular-tree-grammar))
  "Writes a grammar"
  (jzon:with-object writer
    (jzon:write-key writer "nonTerminals")
    (jzon:write-value writer (or (g:non-terminals rtg) (vector)))
    (jzon:write-key writer "productions") ; Maybe we should reverse on read?
    (jzon:write-value writer (or (reverse (g:productions rtg)) (vector)))))

(defmethod jzon:write-value ((writer jzon:writer) (nt g:non-terminal))
  "Writes a non-terminal"
  (jzon:with-object writer
    (jzon:write-key writer "name")
    (jzon:write-value writer (smt:identifier-smt (g:name nt)))
    (jzon:write-key writer "termType")
    (jzon:write-value writer (g:term-type nt))))

(defmethod jzon:write-value ((writer jzon:writer) (prod g:production))
  "Writes a production"
  (jzon:with-object writer
    (jzon:write-key writer "instance")
    (jzon:write-value writer (smt:identifier-smt (g:name (g:instance prod))))
    (jzon:write-key writer "occurrences")
    (jzon:with-array writer
      (loop for occ in (g:occurrences prod)
            doing (jzon:write-value writer (smt:identifier-smt (g:name occ)))))
    (jzon:write-key writer "operator")
    (jzon:write-value writer (smt:identifier-smt (g:name (g:operator prod))))))

(defmethod jzon:write-value ((writer jzon:writer) (rank smt:rank))
  "Writes a rank"
  (jzon:with-object writer
    (jzon:write-key writer "argumentSorts")
    (jzon:write-value writer (smt:argument-sorts rank))
    (jzon:write-key writer "returnSort")
    (jzon:write-value writer (smt:return-sort rank))))

;;;
;;; Term serializers
;;;
(defgeneric write-term-type (writer term)
  (:method :around (writer term)
    (jzon:write-key writer "$termType")
    (jzon:write-value writer (call-next-method)))
  (:method (writer (term smt:application)) "application")
  (:method (writer (term smt:constant)) "variable")
  (:method (writer (term smt:lambda-binder)) "lambda")
  (:method (writer (term smt:match-binder)) "binder")
  (:method (writer (term smt:match-grouper)) "match")
  (:method (writer (term smt:quantifier))
    (?:match term
      ((smt:exists _ _) "exists")
      ((smt:forall _ _) "forall")
      (t (error "Unknown quantifier type!"))))
  (:documentation "Writes a term type key/value pair to WRITER. Expects to be called
inside of an active JSON object block. Primary methods should return the string
value of the term type to write."))

(defmethod jzon:write-value ((writer jzon:writer) (term smt:term))
  "This is the application method!"
  (jzon:with-object writer
    (jzon:write-key writer "name")
    (jzon:write-value writer (smt:identifier-smt (smt:name term)))
    (jzon:write-key writer "returnSort")
    (jzon:write-value writer (smt:sort term))
    (jzon:write-key writer "argumentSorts")
    (jzon:write-value writer (or (smt:child-sorts term) (vector)))
    (jzon:write-key writer "arguments")
    (jzon:write-value writer (or (smt:children term) (vector)))
    (write-term-type writer term)))

(defmethod jzon:write-value ((writer jzon:writer) (term smt:constant))
  "Writes a variable to WRITER"
  (jzon:with-object writer
    (jzon:write-key writer "name")
    (jzon:write-value writer (smt:identifier-smt (smt:name term)))
    (jzon:write-key writer "sort")
    (jzon:write-value writer (smt:sort term))
    (write-term-type writer term)))

(defmethod jzon:write-value ((writer jzon:writer) (term smt:quantifier))
  "Writes a quantifier to WRITER"
  (jzon:with-object writer
    (jzon:write-key writer "bindings")
    (jzon:with-array writer
      (loop for arg in (smt:arguments term)
            for sort in (smt::argument-sorts term)
            do (jzon:write-value writer (smt:variable arg sort))))
    (jzon:write-key writer "child")
    (jzon:write-value writer (first (smt:children term)))
    (write-term-type writer term)))

(defmethod jzon:write-value ((writer jzon:writer) (term smt:lambda-binder))
  "Writes a lambda binder to WRITER"
  (jzon:with-object writer
    (jzon:write-key writer "arguments")
    (jzon:with-array writer
      (map nil #'(lambda (arg) (jzon:write-value writer (smt:identifier-smt arg)))
           (smt:arguments term)))
    (jzon:write-key writer "body")
    (jzon:write-value writer (smt:body term))
    (write-term-type writer term)))

(defmethod jzon:write-value ((writer jzon:writer) (term smt:match-grouper))
  "Writes a match grouper to WRITER"
  (jzon:with-object writer
    (jzon:write-key writer "term")
    (jzon:write-value writer (smt:match-child term))
    (jzon:write-key writer "binders")
    (jzon:with-array writer
      (map nil (*:curry #'jzon:write-value writer) (smt:match-binders term)))
    (write-term-type writer term)))

(defgeneric write-match-patterns (writer pattern)
  (:method (writer (pattern smt:match-pattern-datatype))
    (jzon:write-key writer "operator")
    (jzon:write-value writer (smt:identifier-smt
                              (smt:name (smt:match-pattern-constructor pattern))))
    (jzon:write-key writer "arguments")
    (jzon:with-array writer
        (map nil #'(lambda (arg) (jzon:write-value writer (smt:identifier-smt arg)))
             (smt:match-pattern-variables pattern))))
  (:method (writer (pattern smt:match-pattern-singleton))
    (jzon:write-key writer "operator")
    (jzon:write-value writer nil)
    (jzon:write-key writer "arguments")
    (jzon:write-array writer (smt:identifier-smt
                              (smt:match-pattern-variable pattern))))
  (:documentation "Writes the pattern portion of a match binder to WRITER"))

(defmethod jzon:write-value ((writer jzon:writer) (binder smt:match-binder))
  "Writes a match binder to WRITER"
  (jzon:with-object writer
    (write-match-patterns writer (smt:match-pattern binder))
    (jzon:write-key writer "child")
    (jzon:write-value writer (smt:match-term binder))
    (write-term-type writer binder)))
