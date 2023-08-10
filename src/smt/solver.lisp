;;;;
;;;; SMT solver handling
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defvar *solver*) ; for binding a solver instance dynamically

(defun assert-smt-solver-enabled ()
  "Checks if SMT solving is enabled."
  #+synthkit-disable-smt-solver
  (error "SMT solver support is disabled."))

#+synthkit-disable-smt-solver
(defmacro with-solver ((solver solver-spec) &body body)
  `(let ((solver nil))
     (format t "~&; SMT solving disabled in synthkit~%")
     ,@body))

#-synthkit-disable-smt-solver
(defmacro with-solver ((solver solver-spec) &body body)
  (let ((result (gensym)))
    `(let ((,solver (make-solver ,solver-spec)))
       (unwind-protect
            (progn
              (let ((,result (progn ,@body)))
                (multiple-value-bind (status unread-forms)
                    (finalize-solver ,solver)
                  (values ,result unread-forms status))))
         ;; Ensure the process is terminated.
         ;(format *trace-output* "; Terminating SMT process...~%")
         (cleanup-solver ,solver)))))

(defclass lazy-solver (solver)
  ((solver-spec :reader %solver-spec
                :initarg :solver-spec
                :documentation "Solver specification to initialize"))
  (:documentation "A solver that gets lazily initialized"))

(defmethod initialize-solver ((solver-class (eql 'lazy-solver)) config)
  "Creates a lazy solver"
  (make-instance 'lazy-solver :solver-spec config))

(defmethod finalize-solver ((solver lazy-solver))
  "Finalizes a lazy solver. Does nothing"
  nil)

(defmethod cleanup-solver ((solver lazy-solver))
  "Cleans up a lazy solver. Does nothing"
  nil)

(defun %is-lazy-solver? (solver)
  "Checks if SOLVER is a lazy solver"
  (typep solver 'lazy-solver))

(defmacro with-lazy-solver ((spec) &body body)
  "Lazily initializes a solver specification. Calls to WITH-SOLVER* will initialize it"
  (let ((result-var (gensym)))
    `(let ((*solver* (initialize-solver 'lazy-solver ,spec)))
       (unwind-protect
            (let ((,result-var (progn ,@body)))
              (multiple-value-bind (status unread-forms)
                  (finalize-solver *solver*)
                (values ,result-var unread-forms status)))
           (cleanup-solver *solver*)))))

(defmacro with-solver* ((solver spec) &body body)
  `(if (and (boundp '*solver*) (not (null *solver*)))
       (progn
         (when (%is-lazy-solver? *solver*)
           (setf *solver* (make-solver (%solver-spec *solver*))))
         (let ((,solver *solver*))
           ,@body))
       (with-solver (,solver ,spec)
         ,@body)))

(defun solve (solver-spec &rest assertions)
  "Solves the given SMT query."
  (assert-smt-solver-enabled)
  (with-solver (smt solver-spec)
    (let ((constants (reduce #'append (map 'list #'find-constants assertions))))
      (dolist (c constants)
        (cl-smt-lib:write-to-smt smt `((|declare-const| ,(intern (identifier-smt (name c))) ,(intern (name (sort c))))))))
    (apply #'add-assertion smt assertions)
    (let ((sat (check-sat smt)))
      (prog1
          (if (eql :sat sat) (get-model smt) sat)
        (cl-smt-lib:write-to-smt smt `((|exit|)))))))
