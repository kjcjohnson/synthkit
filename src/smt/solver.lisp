;;;;
;;;; SMT solver handling
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defvar *solver*) ; for binding a solver instance dynamically

#+synthkit-disable-smt-solver
(defmacro with-solver ((solver solver-spec) &body body)
  `(let ((solver nil))
     (format t "~&; SMT solving disabled in synthkit~%")
     ,@body))

#-synthkit-disable-smt-solver
(defmacro with-solver ((solver solver-spec) &body body)
  (let ((form (gensym))
        (result (gensym))
        (status (gensym)))
    `(with-open-stream (,solver (apply #'cl-smt-lib:make-smt (program ,solver-spec) (arguments ,solver-spec)))
       (unwind-protect
            (progn
              (let ((,result (progn
                               ,@body)))
                (close (cl-smt-lib/process-two-way-stream:output ,solver))
                (let ((,status (uiop:wait-process
                                (cl-smt-lib/process-two-way-stream:process ,solver))))
                  (unless (zerop ,status)
                    (error "SMT solver failed with exit status ~S" ,status))
                  (values ,result
                          (loop :for ,form = (cl-smt-lib:read-from-smt ,solver t nil
                                                                       :eof)
                                :while (cl:not (equal :eof ,form))
                                :collect ,form)
                          ,status))))
         ;; Ensure the process is terminated.
         ;(format *trace-output* "; Terminating SMT process...~%")
         (uiop:terminate-process
          (cl-smt-lib/process-two-way-stream:process ,solver))))))

(defmacro with-solver* ((solver spec) &body body)
  `(if (and (boundp '*solver*) (not (null *solver*)))
       (let ((,solver *solver*))
         ,@body)
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
