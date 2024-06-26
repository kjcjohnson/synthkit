;;;;
;;;; Functionality for program execution
;;;;
(in-package #:com.kjcjohnson.synthkit.ast)

;;;
;;; Signalled when the program is semantically invalid (or problem is wrong)
;;;
(define-condition no-applicable-semantics (error)
  ((state :reader state
          :initarg :state
          :documentation "The state that had no applicable semantics")
   (descriptor :reader descriptor
               :initarg :descriptor
               :documentation "The descriptor that had no applicable semantics")
   (node :reader node
         :initarg :node
         :documentation "The node that had no applicable semantics"))
  (:report (lambda (condition stream)
             (format stream
                     "No applicable semantics for state: ~a [~a] and node: ~a~&"
                     (state condition)
                     (smt:identifier-smt (descriptor condition))
                     (print-program (node condition) nil)))))

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
                  (*self-recursion-counter* 0)
                  (*root-input-state* input-state)
                  (*root-input-descriptor* descriptor))
              (if *use-program-compiler*
                  (let ((comp-fn (compile-program semantics descriptor node)))
                    (declare (type transformer comp-fn))
                    (multiple-value-bind (res valid)
                        (funcall comp-fn input-state)
                      (if (or (not (null res)) valid)
                          (setf result res)
                          (go abort-execution))))
                  (setf result
                        (%execute-program semantics descriptor node input-state))))

            ;; --- Validate program compiler ---
            #+ks2-validate-program-compiler
            (let* ((compiled-program (compile-program semantics descriptor node))
                   (comp-ex-result (funcall compiled-program input-state)))
              (unless (smt:state= result comp-ex-result)
                (error "Mismatch in behavior: ~a vs. ~a on ~a"
                       comp-ex-result result node)))
            ;; ---------------------------------

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

(defun %hole-helper (semantics hole descriptor input-state)
  "Runs semantics for a hole, maybe. Returns the output state, or NIL if no semantics"
  (a:when-let (sem-fn (operational-semantics-for-hole
                       semantics descriptor (non-terminal hole)))
    (funcall sem-fn input-state)))

;;;
;;; Internal execution helper
;;;
(defun %execute-program (semantics descriptor node input-state)

  ;;; Short-circuit and yell if we attempt to execute a hole as a program
  (when (typep node 'program-hole)
    (let ((hole-val (%hole-helper semantics node descriptor input-state)))
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
  (error 'no-applicable-semantics
         :node node
         :descriptor descriptor
         :state input-state))
