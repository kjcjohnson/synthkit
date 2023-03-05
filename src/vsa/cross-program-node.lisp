(in-package #:com.kjcjohnson.synthkit.vsa)
(kl/oo:import-classes-from #:kl/c)

;;;
;;; Cartesian product enumerator
;;;
(kl/oo:define-encapsulated-class cross-program-enumerator :extends kl/c::&enumerator
  (private field _started)
  (private field _parent)
  (private field _arity)
  (private field _production)
  (private field _enumerators)

  (public constructor (parent production sets)
          (setf _parent parent)
          (setf _production production)
          (setf _arity (g:arity production))
          (setf _started nil)
          (setf _enumerators (map 'list #'(lambda (s)
                                            (&enumerable:get-enumerator s))
                                  sets)))

  (public property current :set (error "I")
                           :get (make-instance
                                 'ast:program-node 
                                 :production _production
                                 :children (map 'list
                                                #'(lambda (e)
                                                    (&enumerator:current e))
                                                _enumerators)))
  (public move-next ()
          (if _started
              (advance)
              (restart)))

  (private advance ()
           (labels ((adv (enlist)
                      (if (endp enlist)
                          nil
                          (let ((res (&enumerator:move-next (first enlist))))
                            (if (null res)
                                (progn
                                  (&enumerator:reset (first enlist))
                                  (&enumerator:move-next (first enlist))
                                  (adv (rest enlist)))
                                t)))))
             (adv _enumerators)))
  
  (private restart ()
           (setf _started t)
           (every #'(lambda (e)
                      (&enumerator:reset e)
                      (&enumerator:move-next e))
                  _enumerators))
  
  (public reset ()
          (setf _started nil)))

;;;
;;; Cartesian product of program nodes
;;;
(kl/oo:define-encapsulated-class cross-program-node :extends program-node
  (public program-count ()
          (reduce #'*
                  (map 'list #'(lambda (p)
                                 (program-node:program-count p))
                       _sets)
                  :initial-value 1))
  
  (private field _production)
  (private field _sets)

  (public constructor (production sets)
          (setf _production production)
          (setf _sets sets)
          (assert (= (g:arity _production) (length _sets))))

  (public get-enumerator ()
          (cross-program-enumerator:new this _production _sets)))
