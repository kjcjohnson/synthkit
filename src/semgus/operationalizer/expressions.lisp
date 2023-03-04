;;;;
;;;; expressions.lisp - operationalizing expressions
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.operationalizer)

(defgeneric operationalize-smt-expression (expression input-vars output-vars)
  (:documentation "Operationalizes an SMT expression by converting it to Lisp code.
Returns an operationalized expression record")
  (:method ((expression smt::constant) input-vars output-vars)
    "Constants (a.k.a. variables): just return the variable name."
    (declare (ignore input-vars output-vars))
    (smt:name expression))
  ;; Constant types: strings, numbers, bit vectors
  (:method ((expression string) input-vars output-vars)
    (declare (ignore input-vars output-vars))
    expression)
  (:method ((expression number) input-vars output-vars)
    (declare (ignore input-vars output-vars))
    expression)
  (:method ((expression bit-vector) input-vars output-vars)
    (declare (ignore input-vars output-vars))
    expression))
  
(defmethod operationalize-smt-expression ((expression smt::expression)
                                          input-vars
                                          output-vars)
  (let ((name (smt:name expression)))
    (cond
      ((eql name (smt:ensure-identifier "true"))
       't)
      ((eql name (smt:ensure-identifier "false"))
       'nil)
      ((eql name (smt:ensure-identifier "="))
       (let ((arg1 (%operationalize-expression (first (smt:children expression))
                                               input-vars
                                               output-vars))
             (arg2 (%operationalize-expression (second (smt:children expression))
                                               input-vars
                                               output-vars)))
         ;; sets explicitly return T so they aren't treated as guards
         (cond
           ((find arg1 output-vars)
            `(progn
               (setf ,arg1 ,arg2)
               t))
           
           ((find arg2 output-vars)
            `(progn
               (setf ,arg2 ,arg1)
               t))
           
           (t ; Not an assignment - do an equality check instead
            `(smt::core-= ,arg1 ,arg2)))))

      (t
       (let ((fn (smt:get-compiled-function (smt:name expression))))
         (if fn
             `(funcall ,fn ,@(map 'list
                                  #'(lambda (x)
                                      (%operationalize-expression x
                                                                  input-vars
                                                                  output-vars))
                                  (smt:children expression)))
             (error 'operationalization-error
                    :message
                    (format nil
                            "Missing operational definition for: ~a~%"
                            (smt:name expression)))))))))

(defmethod operationalize-smt-expression (expression input-vars output-vars)
  (declare (ignore input-vars output-vars))
   (error 'operationalization-error
          :message
          (format nil
                  "CHC Operationalizer fall-through on: ~a~%"
                  expression)))
