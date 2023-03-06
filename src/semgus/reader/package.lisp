;;;;
;;;; The reader package
;;;;
(defpackage #:com.kjcjohnson.synthkit.semgus.reader
  (:use #:cl)
  (:local-nicknames (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:chc #:com.kjcjohnson.synthkit.semgus.chc)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:u #:com.kjcjohnson.synthkit.utilities)
                    (#:a #:alexandria)))

;;;
;;; Package for problem file symbols
;;;
(defpackage #:com.kjcjohnson.synthkit.semgus.reader.user)
