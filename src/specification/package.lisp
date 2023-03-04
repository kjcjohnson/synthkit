;;;;
;;;; Specification types
;;;;
(defpackage #:com.kjcjohnson.synthkit.specification
  (:use #:cl)
  (:local-nicknames (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:a #:alexandria))
  ;; Specification types
  (:export #:specification
           #:relational-specification
           #:compound-specification
           #:intersection-specification
           #:union-specification
           #:inductive-specification
           #:io-specification)
  ;; Accessors
  (:export #:expression
           #:descriptor
           #:descriptors
           #:components)
  
  ;; Methods
  (:export #:leaf-specification-types
           #:compound-specification-types
           
           #:is-only?
           #:is-only-inductive?
           #:is-only-io?
           #:with-only?
           #:with-only-intersection?
           #:with-only-union?

           #:convert-to-cegis))
