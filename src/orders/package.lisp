;;;;
;;;; Order theory
;;;;
(defpackage #:com.kjcjohnson.synthkit.orders
  (:use #:cl)
  ;; Classes
  (:export #:partial-order
           #:join-semilattice
           #:meet-semilattice
           #:bounded-join-semilattice
           #:bounded-meet-semilattice
           #:lattice
           #:bounded-join-lattice
           #:bounded-meet-lattice
           #:bounded-lattice)
  ;; Operations
  (:export #:join
           #:meet
           #:top
           #:bot
           #:order))

