;;;;
;;;; Legacy solver implementation using cl-smt-lib
;;;;
;;;; A bit broken in some respects
;;;;
(in-package #:com.kjcjohnson.synthkit.smt.solvers)
    
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

(defmethod smt:get-model ((solver process-two-way-stream))
  "Gets an SMT model"
  (flet ((fixup-value (value)
           "Fixes up a value. Rewrites negatives to negatives"
           (?:match value
             ((list - v)
              (- v))
             (v
              v))))

    (cl-smt-lib:write-to-smt solver `((,(intern "get-model"))))
    (let ((output (cl-smt-lib:read-from-smt solver t)))
      ;; CVC4 has the string "model", cvc5 does not
      (when (symbolp (first output))
        (assert (string= "model" (symbol-name (first output))))
        (setf output (rest output)))
      (map 'list #'(lambda (dfn)
                     (destructuring-bind (df-kw name args type value) dfn
                       (declare (ignore args))
                       (assert (string= "define-fun" (symbol-name df-kw)))
                       (cons (smt:variable (smt:ensure-identifier (symbol-name name))
                                       (make-instance 'sort :name (symbol-name type)))
                             (fixup-value value))))
           output))))


