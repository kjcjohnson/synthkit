;;;;
;;;; The SemGuS Verifier verifier
;;;;
;;;; https://github.com/SemGuS-git/Semgus-Verifier
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.verifiers)

(define-condition semgus-verifier-process-error (error) ())

(defclass semgus-verifier ()
  ((program-name :initarg :program-name
                 :reader semgus-verifier-program-name
                 :documentation "The name of the SemGuS Verifier executable")
   (solver-json-dir :initarg :solver-json-dir
                    :reader semgus-verifier-solver-json-dir
                    :documentation "Directory for the solver to write JSON files")
   (verifier-json-dir :initarg :verifier-json-dir
                      :reader semgus-verifier-verifier-json-dir
                      :documentation "Directory for the verifier to read JSON files")
   (program-dir :initarg :program-dir
                :reader semgus-verifier-program-dir
                :documentation "Directory that the verifier is in")
   (problem-filename :initarg :problem-filename
                     :reader semgus-verifier-problem-filename
                     :documentation "Filename to write problems to")
   (solution-filename :initarg :solution-filename
                      :reader semgus-verifier-solution-filename
                      :documentation "Filename to write solutions to")
   (use-shell :initarg :use-shell
              :reader semgus-verifier-use-shell
              :documentation "Whether or not to launch in Bash")
   (options :initarg :options
            :reader semgus-verifier-options
            :documentation "Other options to use in the verifier")
   (print-invalid :initarg :print-invalid
                  :reader semgus-verifier-print-invalid
                  :documentation "Whether or not to print out invalid solutions"))
  (:default-initargs :problem-filename "VER_PROBLEM.json"
                     :solution-filename "VER_SOLUTION.json"
                     :options '("--const-prop" "prog" "--qe"))
  (:documentation "A verifier implementation using the 'SemGuS Verifier' program. There
are directory configurations for both the solver and the verifier; on a homogeneous
system, these would be the same, but on (e.g.) Windows and WSL, these files paths
may point to the same physical directory but with different names"))

(defparameter +default-exe-name+ #P"/mnt/d/dev/semgus/Semgus-Verifier/verifier.exe")
(defparameter +default-dir-location+ #P"d:/dev/semgus/Semgus-Verifier/")
(defparameter +solver-temp-location+ #P"d:/temp/verifier/")
(defparameter +verifier-temp-location+ #P"/mnt/d/temp/verifier/")

(defmethod initialize-instance :after ((v semgus-verifier) &key)
  "Initializes the SemGuS Verifier"
  (let* ((executable (or (uiop:getenv "SEMGUS_VERIFIER_EXE")
                         +default-exe-name+))
         (directory (or (uiop:getenv "SEMGUS_VERIFIER_DIR")
                        +default-dir-location+))
         (s-json-dir (or (uiop:getenv "SEMGUS_VERIFIER_SOLVER_JSON_DIR")
                         (uiop:getenv "SEMGUS_VERIFIER_JSON_DIR")
                         +solver-temp-location+))
         (v-json-dir (or (uiop:getenv "SEMGUS_VERIFIER_VERIFIER_JSON_DIR")
                         (uiop:getenv "SEMGUS_VERIFIER_JSON_DIR")
                         +verifier-temp-location+))
         (yes-use-shell (uiop:getenv "SEMGUS_VERIFIER_USE_SHELL"))
         (no-no-shell (uiop:getenv "SEMGUS_VERIFIER_NO_SHELL"))
         (use-shell (cond (yes-use-shell)
                          (no-no-shell nil)
                          ((uiop:os-windows-p)) ; Verifier run through WSL
                          (t nil)))
         (print-invalid (*:true (uiop:getenv "SEMGUS_VERIFIER_PRINT_INVALID"))))

    (u:set-slot-if-unbound v 'program-name executable)
    (u:set-slot-if-unbound v 'program-dir directory)
    (u:set-slot-if-unbound v 'solver-json-dir s-json-dir)
    (u:set-slot-if-unbound v 'verifier-json-dir v-json-dir)
    (u:set-slot-if-unbound v 'use-shell use-shell)
    (u:set-slot-if-unbound v 'print-invalid print-invalid)))

(defparameter *semgus-verifier-instance* nil)

(defun initialize-semgus-verifier ()
  "Initializes the SemGuS Verifier instance"
  (setf *semgus-verifier-instance* (make-instance 'semgus-verifier)))

(defmethod semgus:forced-semgus-verifier ()
  "Gets the forced semgus verifier instance"
  (when (null *semgus-verifier-instance*)
    (initialize-semgus-verifier))
  *semgus-verifier-instance*)

(defun %build-verifier-command (verifier)
  "Builds a command for VERIFIER"
  (let* ((executable (semgus-verifier-program-name verifier))
         (problem (merge-pathnames (semgus-verifier-problem-filename verifier)
                                   (semgus-verifier-verifier-json-dir verifier)))
         (solution (merge-pathnames (semgus-verifier-solution-filename verifier)
                                    (semgus-verifier-verifier-json-dir verifier)))
         (cmd (list*
               (namestring executable)
               "--verify"
               (namestring problem)
               (namestring solution)
               (semgus-verifier-options verifier))))

    (if (semgus-verifier-use-shell verifier)
        (list "bash" "-c" (str:join #\Space cmd))
        cmd)))

(defun %run-semgus-verifier (verifier problem program spec)
  "Runs the SemGuS Verifier"
  ;;
  ;; RUN SYNCHRONOUSLY - we were seeing a memory leak that made interactive mode fail
  ;;
  (let* ((tmpdir (semgus-verifier-solver-json-dir verifier))
         (pf (merge-pathnames (semgus-verifier-problem-filename verifier) tmpdir))
         (sf (merge-pathnames (semgus-verifier-solution-filename verifier) tmpdir)))
    (semgus:write-problem pf (semgus:replace-specification problem spec) :json)
    (semgus:write-program sf (semgus:term-name problem) program :json))

  (let ((output)
        (proc))
    (unwind-protect
         (progn
           (setf proc
                 (uiop:launch-program (%build-verifier-command verifier)
                                      :directory (semgus-verifier-program-dir verifier)
                                      :output :stream))
           (loop for line = (read-line (uiop:process-info-output proc) nil :eof)
                 until (eql line :eof)
                 doing (push line output))
           (uiop:wait-process proc))
      (when (and proc (uiop:process-alive-p proc))
        (uiop:terminate-process proc :urgent t)))

    (when (zerop (length output))
      (error 'semgus-verifier-process-error "Verifier did not produce a response"))

    (str:string-case (str:downcase (first output))
      ("valid" :valid)
      ("invalid" :invalid)
      (otherwise (format *trace-output* "; Bad response from verifier: ~a" output)
                 :unknown))))

(defun %filter-specification-inductive (spec)
  "Filters a specification to an inductive specification"
  (spec:filter-examples spec #'spec:is-only-inductive? :key #'identity))

(defun %filter-specification-relational (spec)
  "Filters a specification to a relational specification"
  (spec:filter-examples spec #'spec:is-relational? :key #'identity))

(defparameter *quick-check-count* 0)
(defparameter *full-check-count* 0)

(u:declare-timed-section *full-check-section* "Timing for calling the full verifier")

(defmethod semgus:verify-program ((verifier semgus-verifier) spec problem program
                                  &key produce-cex)
  "Verifies a semgus problem"
  (declare (ignore produce-cex))
  ;;
  ;; Check IO specs first for invalid, because those are fast
  ;;
  (let ((io (%filter-specification-inductive spec)))
    (let ((semgus:*force-semgus-verifier* nil))
      (unless (semgus:check-program problem program :specification io)
        (incf *quick-check-count*)
        (return-from semgus:verify-program :invalid))))
  ;;
  ;; Otherwise fall back to the full verifier
  ;;
  (incf *full-check-count*)
  (let ((res (u:with-timed-section (*full-check-section*)
               (%run-semgus-verifier verifier problem program
                                     (%filter-specification-relational
                                      (semgus:specification problem))))))
    (if (eql res :valid)
        (progn
          (format t "~&; --- FOUND VALID [~a quick checks, ~a full checks]~%"
                  *quick-check-count* *full-check-count*)
          (format t "~&;      ~~~~ ~,2fs in full verifier~%"
                  (u:get-timed-section-real-time *full-check-section*)))
        (when (semgus-verifier-print-invalid verifier)
          (format t "~&; --- FOUND INVALID: ~a~%" program)))
    res))
