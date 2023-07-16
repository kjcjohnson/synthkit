;;;;
;;;; Order operations
;;;;
(in-package #:com.kjcjohnson.synthkit.orders)

(defgeneric order (partial-order &rest elements)
  (:documentation "True if all ELEMENTS are in order based on PARTIAL-ORDER"))

(defgeneric join (join-semilattice &rest elements)
  (:documentation "Produces an element from JOIN-SEMILATTICE as the join of ELEMENTS"))

(defgeneric meet (meet-semilattice &rest elements)
  (:documentation "Produces an element from MEET-SEMILATTICE as the meet of ELEMENTS"))

(defgeneric top (bounded-meet-lattice)
  (:documentation "Produces the top element of BOUNDED-MEET-LATTICE"))

(defgeneric bot (bounded-join-lattice)
  (:documentation "Produces the bottom element of BOUNDED-JOIN-LATTICE"))
