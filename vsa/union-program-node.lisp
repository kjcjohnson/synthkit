(in-package #:com.kjcjohnson.synthkit.vsa)
(kl/oo:import-classes-from #:kl/c)

;;;
;;; Union program node enumerator
;;;
(kl/oo:define-encapsulated-class union-program-enumerator :extends kl/c::&enumerator
  (private field _parent)
  (private field _index)
  (private field _enumerator)

  (public constructor (parent)
          (setf _parent parent)
          (reset))

  (public property current :get (&enumerator:current _enumerator) :set (error "I"))
  
  (public reset ()
          (setf _index -1)
          (setf _enumerator nil))

  (public move-next ()
          (if (or (= _index -1)
                  (not (&enumerator:move-next _enumerator)))
              (progn
                (incf _index)
                (let ((next-node
                        (kl/oo:method-invoke _parent :nth-program _index)))
                  (if (null next-node)
                      nil
                      (progn
                        (setf _enumerator (&enumerable:get-enumerator next-node))
                        (move-next)))))
              t)))

;;;
;;; A program node representing a union of programs
;;;
(kl/oo:define-encapsulated-class union-program-node :extends program-node
  (public program-count ()
          (reduce #'+
                  (map 'list #'(lambda (p)
                                 (program-node:program-count p))
                       _programs)))
  
  ;; Set of program nodes wrapped in this node
  (private field _programs)

  ;; Gets the program nodes being unioned
  (public property programs :get _programs :set (error "Immutable"))

  ;; Gets the nth program node in the union
  (public nth-program (n)
          (nth n _programs))
  
  ;; Creates a new union program node
  (public constructor (programs)
          (setf _programs programs))

  ;; Enumerator over all programs in this node
  (public get-enumerator ()
          (union-program-enumerator:new this)))
