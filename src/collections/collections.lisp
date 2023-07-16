;;;;
;;;; Collections interfaces - pull out into separate library if this works well
;;;;
(in-package #:com.kjcjohnson.synthkit.collections)

;;; Creation
(defgeneric new (descriptor &key test)
  (:documentation "Creates a new collection from DESCRIPTOR"))

;;; Collections
(defgeneric add (collection item)
  (:documentation "Adds ITEM to COLLECTION"))

(defgeneric clear (collection)
  (:documentation "Clears all items from COLLECTION"))

(defgeneric contains (collection item)
  (:documentation "Checks if COLLECTION contains ITEM"))

(defgeneric count (collection) ; TODO: maybe take a predicate here?
  (:documentation "Gets the count of elements in COLLECTION"))

(defgeneric remove (collection item)
  (:documentation "Removes ITEM from COLLECTION"))

;;; Addressable collections
(defgeneric get (collection index)
  (:documentation "Gets the item at INDEX from COLLECTION"))

(defgeneric (setf get) (item collection index)
  (:documentation "Sets INDEX to ITEM in COLLECTION"))

;;; Maps
(defgeneric contains-key (map key)
  (:documentation "Checks if MAP contains KEY"))

(defgeneric contains-value (map value)
  (:documentation "Checks if MAP contains VALUE"))
