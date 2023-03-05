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
;;; Counts number of programs executed
;;;
(defvar *execution-counter* 0)

;;;
;;; Handles simple unbounded-recursion detection
;;;
(defvar *self-recursion-counter* 0
  "Counts recursion depth to find probably-non-terminating programs.")
(defparameter *self-recursion-limit* 200)


;;;
;;; Public execution interface
;;;
(defun execute-program (semantics descriptor node input-state)
  (incf *execution-counter*)
  (let (result abort-exit)
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
          ;(format *trace-output* "; ABORT EXIT: ~a~%" node)
          (values nil nil))
        (values (canonicalize-result semantics result) t))))

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

;;;
;;; Internal execution helper
;;;
(defun %execute-program (semantics descriptor node input-state)

  ;;; Short-circuit and yell if we attempt to execute a hole as a program
  (when (typep node 'program-hole)
    (error "Attempting to execute hole: ~a" node))
  
  (dolist (calling-card (operational-semantics-for-production semantics
                                                              descriptor
                                                              (production node)))
    (multiple-value-bind (result valid)
        (funcall (build-semantics-from-calling-card semantics calling-card node)
                 input-state)
      ;;(break)
      (when (or (not (null result)) valid)
        (return-from %execute-program (values result t)))))
  (error "No applicable semantics: ~a" (g:name (operator node))))
