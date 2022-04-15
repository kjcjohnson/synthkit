(in-package #:com.kjcjohnson.synthkit.vsa)
(kl/oo:import-classes-from #:kl/c)

;;;
;;; A trivial enumerator for single programs
;;;
(kl/oo:define-encapsulated-class leaf-program-enumerator :extends kl/c::&enumerator
  (private field _started)
  (private field _parent)

  (public constructor (parent)
          (setf _started nil)
          (setf _parent parent))

  (public property current :get (kl/oo:property-invoke _parent :program))

  (public move-next ()
          (if _started
              nil
              (setf _started t)))

  (public reset ()
          (setf _started nil)))

;;;
;;; A program node representing a single concrete program
;;;
(kl/oo:define-encapsulated-class leaf-program-node :extends program-node
  (public program-count () 1)
  
  ;; The program represented by this leaf
  (private field _program)

  ;; Gets the program from this node
  (public property program :get _program :set (error "Immutable"))

  ;; Creates a new leaf program node for the given program
  (public constructor (program)
          (setf _program program))

  ;; Gets an enumerator over all programs
  (public get-enumerator ()
          (leaf-program-enumerator:new this)))
