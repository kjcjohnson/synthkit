;;;;
;;;; Types of orders
;;;;
(in-package #:com.kjcjohnson.synthkit.orders)

(defclass partial-order ()
  ()
  (:documentation "A partial order."))

(defclass join-semilattice (partial-order)
  ()
  (:documentation "A join semilattice"))

(defclass meet-semilattice (partial-order)
  ()
  (:documentation "A meet semilattice"))

(defclass bounded-join-semilattice (join-semilattice)
  ()
  (:documentation "A join semilattice with a bottom element"))

(defclass bounded-meet-semilattice (meet-semilattice)
  ()
  (:documentation "A meet semilattice with a top element"))

(defclass lattice (join-semilattice meet-semilattice)
  ()
  (:documentation "A lattice"))

(defclass bounded-join-lattice (lattice bounded-join-semilattice)
  ()
  (:documentation "A lattice with a bottom element"))

(defclass bounded-meet-lattice (lattice bounded-meet-semilattice)
  ()
  (:documentation "A lattice with a top element"))

(defclass bounded-lattice (bounded-join-lattice bounded-meet-lattice)
  ()
  (:documentation "A lattice with a top and a bottom element"))
