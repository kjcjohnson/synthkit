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
