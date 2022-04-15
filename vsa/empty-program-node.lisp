(in-package #:com.kjcjohnson.synthkit.vsa)
(kl/oo:import-classes-from #:kl/c)

;;;
;;; Empty enumerator
;;;
(kl/oo:define-encapsulated-class empty-program-enumerator :extends kl/c::&enumerator
  (public constructor ())
  (public property current :get nil :set (error "I"))
  (public move-next () nil)
  (public reset ()))

;;;
;;; Empty program node
;;;
(kl/oo:define-encapsulated-class empty-program-node :extends program-node
  (public constructor ())
  (public program-count () 0)
  (public get-enumerator ()
          (empty-program-enumerator:new)))
