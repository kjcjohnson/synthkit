;;;;
;;;; CHCs package
;;;;
(defpackage #:com.kjcjohnson.synthkit.semgus.chc
  (:use #:cl)
  (:shadow #:symbol-name)
  ;; CHCs
  (:export #:chc
           #:symbol-table)
  ;; Symbol tables
  (:export #:symbol-entry
           #:symbol-name #:symbol-sort #:symbol-index

           #:symbol-table
           #:input-symbols #:output-symbols
           #:term-symbol
           #:auxiliary-symbols #:child-symbols))
