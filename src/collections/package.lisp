;;;;
;;;; Collections packages
;;;;
(defpackage #:com.kjcjohnson.synthkit.collections
  (:import-from #:cl
                #:defgeneric
                #:defclass
                #:defun
                #:defstruct
                #:setf
                #:&key)

  ;; Classes
  (:export #:collection
           #:set
           #:map)
  
  ;; Creation
  (:export #:new)

  ;; General Collections
  (:export #:add #:clear #:contains #:count #:remove)

  ;; Indexable collections
  (:export #:get)

  ;; Maps
  (:export #:contains-key #:contains-value)
  
  ;; Key-value pairs
  (:export #:kvp #:key #:value))
  
(defpackage #:com.kjcjohnson.synthkit.collections.impl
  (:use #:cl)
  (:local-nicknames (#:c #:com.kjcjohnson.synthkit.collections)
                    (#:* #:serapeum/bundle)))
