;;;;
;;;; Legacy solver implementation using cl-smt-lib
;;;;
;;;; A bit broken in some respects
;;;;
(in-package #:com.kjcjohnson.synthkit.smt.solvers)

(defclass cl-smt-lib-solver (smt:solver process-two-way-stream)
  ()
  (:documentation "A CL-SMT-LIB-based solver"))

;;; Should be solver-configuration
(defmethod smt:make-solver ((solver-config smt:solver*))
  "Creates a solver. Currently just the cl-smt-lib-based solver"
  (smt:initialize-solver 'cl-smt-lib-solver solver-config))

(defmethod smt:initialize-solver ((solver-class (eql 'cl-smt-lib-solver)) config)
  "Initializes a cl-smt-lib-based solver"
  (let ((process (uiop:launch-program (cons (smt:program config)
                                            (smt:arguments config))
                                      :input :stream
                                      :output :stream)))
    (make-instance 'cl-smt-lib-solver
      :input (uiop:process-info-output process)
      :output (uiop:process-info-input process)
      :process process)))

(defmethod smt:finalize-solver ((solver cl-smt-lib-solver))
  "Finalizes the cl-smt-lib-based solver"
  (close (cl-smt-lib/process-two-way-stream:output solver))
  (let ((status (uiop:wait-process
                 (cl-smt-lib/process-two-way-stream:process solver))))
    (unless (zerop status)
      (error "SMT solver failed with exit status ~S" status))
    (values
     status
     (loop for form = (cl-smt-lib:read-from-smt solver t nil :eof)
           while (not (equal :eof form))
           collect form))))

(defmethod smt:cleanup-solver ((solver cl-smt-lib-solver))
  "Cleans up an cl-smt-lib-based solver"
  (close solver)
  (let ((solver-proc-info (cl-smt-lib/process-two-way-stream:process solver)))
    (uiop:terminate-process solver-proc-info :urgent nil)))


(defmethod smt:dump ((solver process-two-way-stream) string)
  "Dumps raw SMT commands in STRING to SOLVER"
  (when cl-smt-lib:*smt-debug*
    (format cl-smt-lib:*smt-debug* "~&;; WRITE-TO-SMT~%")
    (format cl-smt-lib:*smt-debug* "~a~%" string)
    (finish-output cl-smt-lib:*smt-debug*))
  (format solver "~a~%" string)
  (finish-output solver))

(defmethod smt:reset-solver ((solver process-two-way-stream))
  "Executes a reset command"
  (cl-smt-lib:write-to-smt solver `((,(intern "reset")))))

(defmethod smt:set-logic ((solver process-two-way-stream) logic)
  "Sets the logic (and resets first)"
  (smt:reset-solver solver)
  (cl-smt-lib:write-to-smt solver `((,(intern "set-logic")
                                     ,(intern logic)))))

(defmethod smt:push-scope ((solver process-two-way-stream) &optional (levels 1))
  "Pushes an SMT scope"
  (cl-smt-lib:write-to-smt solver `((,(intern "push") ,levels))))

(defmethod smt:pop-scope ((solver process-two-way-stream) &optional (levels 1))
  "Pops an SMT scope"
  (cl-smt-lib:write-to-smt solver `((,(intern "pop") ,levels))))

(defmethod smt:add-assertion ((solver process-two-way-stream) &rest assertions)
  "Adds ASSERTIONS to SOLVER"
  (let ((as (intern "assert")))
    (dolist (a assertions)
      (cl-smt-lib:write-to-smt solver `((,as ,(smt:to-smt a)))))))

(defmethod smt:declare-constant ((solver process-two-way-stream) &rest constants)
  "Declares constants in list CONSTANTS"
  (dolist (c constants)
    (cl-smt-lib:write-to-smt solver `((,(intern "declare-const")
                                       ,(intern (smt:identifier-smt (smt:name c)))
                                       ,(intern (smt:name (smt:sort c))))))))

(defmethod smt:check-sat ((solver process-two-way-stream))
  "Checks satisfibility"
  (cl-smt-lib:write-to-smt solver `((,(intern "check-sat"))))
  (let ((result (cl-smt-lib:read-from-smt solver t)))
    (cond ((eql (intern "sat") result) :sat)
          ((eql (intern "unsat") result) :unsat)
          (t :unknown))))

(defmethod smt:read-model ((solver process-two-way-stream))
  "Reads an SMT model from the solver"
  (labels ((fixup-value (value)
             "Fixes up a value. Rewrites negatives to negatives"
             (?:match value
               ((list '- v)
                (- v))
               (v
                v)))
           (parse-expr (expr known-symbols)
             "Very basic expression parser...doesn't handle special forms"
             (?:match expr
               ((list '- (type integer))
                (- (second expr)))
               ((list* fn args)
                (let ((children (map 'list (a:rcurry #'parse-expr known-symbols) args)))
                  (make-instance 'smt::expression
                                 :name (smt:ensure-identifier (symbol-name fn))
                                 :arity (length children)
                                 :children children
                                 :child-sorts (map 'list #'smt:sort children)
                                 :sort nil)))
               ((type integer)
                (smt:$literal expr smt:*int-sort*))
               ((type string)
                (smt:$literal expr smt:*string-sort*))
               ((type symbol)
                (a:if-let (s (assoc expr known-symbols))
                  (smt:variable
                   (smt:ensure-identifier (symbol-name expr))
                   (smt:ensure-sort (symbol-name (second s))))
                  (smt:variable
                   (smt:ensure-identifier (symbol-name expr))
                   nil))))))

    (let ((output (cl-smt-lib:read-from-smt solver t)))
      ;; CVC4 has the string "model", cvc5 does not
      (when (symbolp (first output))
        (assert (string= "model" (symbol-name (first output))))
        (setf output (rest output)))
      (map 'list #'(lambda (dfn)
                     (destructuring-bind (df-kw name args type value) dfn
                       (assert (string= "define-fun" (symbol-name df-kw)))
                       (if (null args)
                           (cons (smt:variable
                                  (smt:ensure-identifier (symbol-name name))
                                  (make-instance 'smt:sort
                                                 :name (symbol-name type)))
                                 (fixup-value value))
                           (smt::function-declaration
                            (smt:ensure-identifier (symbol-name name))
                            (map 'list
                                 (a:compose #'smt:ensure-sort #'symbol-name #'second)
                                 args)
                            (smt:ensure-sort (symbol-name type))
                            (map 'list
                                 (a:compose #'smt:ensure-identifier
                                            #'symbol-name
                                            #'first)
                                 args)
                            (parse-expr value args)))))
           output))))

(defmethod smt:get-model ((solver process-two-way-stream))
  "Gets an SMT model"
  (cl-smt-lib:write-to-smt solver `((,(intern "get-model"))))
  (smt:read-model solver))
