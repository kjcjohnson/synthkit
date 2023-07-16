;;;;
;;;; Functionality for program execution
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

;;;
;;; Allows exiting a program early
;;;
(defvar *program-execution-exit-hook*)
(defun abort-program-execution ()
  (funcall *program-execution-exit-hook*))

;;;
;;; Handles simple unbounded-recursion detection
;;;
(defvar *self-recursion-counter* 0
  "Counts recursion depth to find probably-non-terminating programs.")
(defparameter *self-recursion-limit* 200)

;;;
;;; Debugging controls
;;;
(defvar *exe-debug* nil "Set to T when execution debugging is triggered")
(defvar *exe-level* 0 "Level of nested program execution")
(defvar *exe-debug-match* nil "If non-nil, must be a string that, when contained in the
serialization of a program node, triggers ``*EXE-DEBUG*`` to be set to T.")
(declaim (type (or null string) *exe-debug-match*))

;;;
;;; Public execution interface
;;;
(defun execute-program (semantics descriptor node input-state)
  (incf *execution-counter*)
  (unwind-protect
       (let (result abort-exit)
         (when (and *exe-debug-match*
                    (str:contains? *exe-debug-match* (format nil "~a" node)))
           (format *trace-output* "=============================~%")
           (setf *exe-debug* t))
         (tagbody
            (let ((*program-execution-exit-hook* #'(lambda () (go abort-execution)))
                  (*self-recursion-counter* 0))
              (setf result (%execute-program semantics descriptor node input-state)))
            (go finish-execution)
          abort-execution
            (setf abort-exit t)
          finish-execution)
         (if abort-exit
             (progn
               ;; (format *trace-output* "; ABORT EXIT: ~a~%" node)
               (values nil nil))
             (values (canonicalize-result semantics result) t)))
    (setf *exe-debug* nil)))

;;;
;;; Building semantic functions
;;;
(defun build-semantics-with-node-id (semantics descriptor node node-id)
  "Builds a semantic function when a specific node id is requested"
  (let ((child-node
          (if (eql node-id :self)
              node
              (nth node-id (children node))))
        (self-recursion-initial-input-state nil))
    #'(lambda (input-state)
        (when (eql node-id :self)
          (cond
            ((> *self-recursion-counter* *self-recursion-limit*)
             (abort-program-execution))
            ((null self-recursion-initial-input-state)
             (setf self-recursion-initial-input-state input-state))
            ((smt:state= self-recursion-initial-input-state input-state)
             (abort-program-execution)))
          (incf *self-recursion-counter*))
        (multiple-value-prog1
            (%execute-program semantics descriptor child-node input-state)
          (when (eql node-id :self)
            (decf *self-recursion-counter*))))))

(defun build-semantics-without-node-id (semantics descriptor)
  "Builds a semantic function when a specific node id is not requested"
  #'(lambda (node)
      #'(lambda (input-state)
          (%execute-program semantics descriptor node input-state))))

(defun build-semantics-from-calling-card (semantics calling-card node)
  "Builds a semantic function from a calling card"
  (let ((sem-fns
          (loop for request in (semantics-descriptor-requests calling-card)
                for descriptor = (semantics-descriptor-request-descriptor request)
                for node-id = (semantics-descriptor-request-node-id request)
                collecting
                (if node-id
                    (build-semantics-with-node-id semantics
                                                  descriptor
                                                  node
                                                  node-id)
                    (build-semantics-without-node-id semantics
                                                     descriptor)))))
    (funcall (semantic-builder-function calling-card)
             sem-fns
             node
             (children node))))

(defun %hole-helper (semantics descriptor input-state)
  "Runs semantics for a hole, maybe. Returns the output state, or NIL if no semantics"
  (a:when-let (sem-fn (operational-semantics-for-hole semantics descriptor))
    (funcall sem-fn input-state)))

;;;
;;; Internal execution helper
;;;
(defun %execute-program (semantics descriptor node input-state)

  ;;; Short-circuit and yell if we attempt to execute a hole as a program
  (when (typep node 'program-hole)
    (let ((hole-val (%hole-helper semantics descriptor input-state)))
      (if (null hole-val)
          (error "Attempting to execute hole: ~a" node)
          (return-from %execute-program (values hole-val t)))))

  (dolist (calling-card (operational-semantics-for-production semantics
                                                              descriptor
                                                              (production node)))
    (when *exe-debug* (format *trace-output* "~aCALL: ~a [~a]~%" (make-string *exe-level* :initial-element #\Space) node input-state))
    (multiple-value-bind (result valid)
        (let ((*exe-level* (1+ *exe-level*)))
          (funcall (build-semantics-from-calling-card semantics calling-card node)
                   input-state))
        (when *exe-debug* (format *trace-output* "~aRETN: ~a [~:[INVALID~;VALID~]]~%" (make-string *exe-level* :initial-element #\Space) result valid))
        (when (or (not (null result)) valid)
          (return-from %execute-program (values result t)))))
  (error "No applicable semantics: ~a [descriptor: ~a]"
         (g:name (operator node)) descriptor))
