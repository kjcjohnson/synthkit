;;;;
;;;; SMT solver handling
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defvar *solver*) ; for binding a solver instance dynamically

(defun assert-smt-solver-enabled ()
  "Checks if SMT solving is enabled."
  #+synthkit-disable-smt-solver
  (error "SMT solver support is disabled."))

(defun init-solver (solver-spec)
  "Initializes an SMT solver"
  (assert-smt-solver-enabled)
  (apply #'cl-smt-lib:make-smt (program solver-spec) (arguments solver-spec)))

(defun deinit-solver (solver)
  "Deinitializes an SMT solver. Returns the solver status code and any unread forms"
  (close (cl-smt-lib/process-two-way-stream:output solver))
  (let ((status (uiop:wait-process
                 (cl-smt-lib/process-two-way-stream:process solver))))
    (unless (zerop status)
      (error "SMT solver failed with exit status ~S" status))
    (values
     status
     (loop for form = (cl-smt-lib:read-from-smt solver t nil :eof)
           while (cl:not (equal :eof form))
           collect form))))

(defun cleanup-solver (solver)
  "Final solver cleanup steps."
  (uiop:terminate-process
   (cl-smt-lib/process-two-way-stream:process solver))
  (close solver))

#+synthkit-disable-smt-solver
(defmacro with-solver ((solver solver-spec) &body body)
  `(let ((solver nil))
     (format t "~&; SMT solving disabled in synthkit~%")
     ,@body))

#-synthkit-disable-smt-solver
(defmacro with-solver ((solver solver-spec) &body body)
  (let ((result (gensym)))
    `(let ((,solver (init-solver ,solver-spec)))
       (unwind-protect
            (progn
              (let ((,result (progn ,@body)))
                (multiple-value-bind (status unread-forms)
                    (deinit-solver ,solver)
                  (values ,result unread-forms status))))
         ;; Ensure the process is terminated.
         ;(format *trace-output* "; Terminating SMT process...~%")
         (cleanup-solver ,solver)))))

(defclass lazy-solver ()
  ((solver-spec :reader %solver-spec
                :initarg :solver-spec
                :documentation "Solver specification to initialize"))
  (:documentation "A solver that gets lazily initialized"))

(defun %is-lazy-solver? (solver)
  "Checks if SOLVER is a lazy solver"
  (typep solver 'lazy-solver))

(defmacro with-lazy-solver ((spec) &body body)
  "Lazily initializes a solver specification. Calls to WITH-SOLVER* will initialize it."
  (let ((result-var (gensym)))
    `(let ((*solver* (make-instance 'lazy-solver :solver-spec ,spec)))
       (unwind-protect
            (let ((,result-var (progn ,@body)))
              (if (%is-lazy-solver? *solver*)
                  ,result-var
                  (multiple-value-bind (status unread-forms)
                      (deinit-solver *solver*)
                    (values ,result-var unread-forms status))))
         (unless (%is-lazy-solver? *solver*)
           (cleanup-solver *solver*))))))

(defmacro with-solver* ((solver spec) &body body)
  `(if (and (boundp '*solver*) (not (null *solver*)))
       (progn
         (when (%is-lazy-solver? *solver*)
           (setf *solver* (init-solver (%solver-spec *solver*))))
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
    (apply #'add smt assertions)
    (let ((sat (check-sat smt)))
      (prog1
          (if (eql :sat sat) (get-model smt) sat)
        (cl-smt-lib:write-to-smt smt `((|exit|)))))))
