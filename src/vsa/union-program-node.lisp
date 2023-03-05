(in-package #:com.kjcjohnson.synthkit.vsa)
(kl/oo:import-classes-from #:kl/c)
(kl/oo:import-classes-from #:com.kjcjohnson.synthkit.vsa)

;;;
;;; Union program node enumerator
;;;
(kl/oo:define-encapsulated-class union-program-enumerator :extends kl/c::&enumerator
  (private field _parent)
  (private field _index)
  (private field _enumerator)
  (private field _program-list)
  (private field _next-pointer)

  (public constructor (parent program-list)
          (setf _parent parent)
          (setf _program-list program-list)
          (reset))

  (public property current :get (&enumerator:current _enumerator) :set (error "I"))

  (public reset ()
          (setf _next-pointer (cons :sentinel _program-list))
          (setf _index -1)
          (setf _enumerator nil))

  (public move-next ()
          (if (or (= _index -1)
                  (not (&enumerator:move-next _enumerator)))
              (progn
                (incf _index)
                (setf _next-pointer (cdr _next-pointer))
                (let ((next-node
                        ;; (kl/oo:method-invoke _parent :nth-program _index)))
                        (car _next-pointer)))
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
          (setf _programs (remove-if #'(lambda (p)
                                         (typep p 'empty-program-node))
                                     programs)))

  ;; Enumerator over all programs in this node
  (public get-enumerator ()
          (union-program-enumerator:new this _programs)))