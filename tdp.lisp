;;;
;;; Top-down propagation framework
;;;
(in-package #:com.kjcjohnson.synthkit.tdp)

(defclass tdp-algorithm ()
  ((learning-tasks
    :initform (make-hash-table)
    :reader learning-tasks)
   (specialized-learning-tasks
    :initform (make-hash-table)
    :reader specialized-learning-tasks)))

(defclass specialized-learning-task ()
  ((specializer :initform (error "Specializer is required.")
                :initarg :specializer
                :reader specializer)
   (learning-task :initform (error "Learning task is required.")
                  :initarg :specializer
                  :reader specializer)))

(defun add-specialized-learning-task (algo nt spec lt)
  "Adds a specialized learning task for the given non-terminal.
   SPEC should be a function taking problem and info parameters, and LT taking algo, problem, nt, and info."
  (push (cons spec lt) (gethash nt (specialized-learning-tasks algo))))

(defun default-learning-task (algo problem nt info)
  "The default recursive learning task, which finds and invokes specialized tasks."
  (dolist (slt (gethash nt (specialized-learning-tasks algo)))
    ;;(format t "~A ~A~%" nt slt)
    (when (funcall (car slt) problem info)
      (return-from default-learning-task (funcall (cdr slt) algo problem nt info))))
  (loop for x being the hash-keys of (specialized-learning-tasks algo)
        doing (format t "k: ~A~%" x))
  (error "No applicable specialized learning task. NT: ~A, Info: ~A" nt info))

(defun invoke-learning-task (algo problem nt info)
  "Finds and invokes the learning task for the given non-terminal."
  (let ((lt (gethash nt (learning-tasks algo))))
    (if (null lt)
        (default-learning-task algo problem nt info)
        (funcall lt algo problem nt info))))

(defun tdp (algo problem info)
  (let ((start (g:initial-non-terminal (semgus:grammar problem))))
    (invoke-learning-task algo problem start info)))
  
