;;;;
;;;; Maps
;;;;
(in-package #:com.kjcjohnson.synthkit.collections.impl)

;;;
;;; Alist maps
;;;
(defclass alist-map (c:map)
  ((map-alist :accessor map-alist
              :initarg :map-alist
              :type list
              :documentation "Underlying alist for this map")
   (key-test :accessor key-test
             :initarg :key-test
             :type function
             :documentation "Test for keys in this map")
   (value-test :accessor value-test
               :initarg :value-test
               :type function
               :documentation "Test for values in this map"))
  (:default-initargs :map-alist nil :key-test #'eql :value-test #'eql)
  (:documentation "A map using an alist as the underlying storage"))

(defmethod c:new ((descriptor (eql :alist-map)) &key (test #'eql) (value-test #'eql))
  "Creates a fresh alist map"
  (make-instance 'alist-map :key-test test :value-test value-test))

(defmethod c:add ((map alist-map) kvp)
  "Adds a key-value pair to this map"
  (declare (type c:kvp kvp))
  (setf (c:get map (c:key kvp)) (c:value kvp))
  map)

(defmethod c:clear ((map alist-map))
  "Clears MAP."
  (setf (map-alist map) nil)
  map)

(defmethod c:contains ((map alist-map) kvp)
  "Checks if MAP contains the key-value pair KVP"
  (declare (type c:kvp kvp))
  (multiple-value-bind (value found?)
      (c:get map (c:key kvp))
    (and found? (funcall (value-test map) value (c:value kvp)))))

(defmethod c:count ((map alist-map))
  "Counts the entries in this map"
  ;; N.B.: keep track of this separately
  (length (map-alist map)))

