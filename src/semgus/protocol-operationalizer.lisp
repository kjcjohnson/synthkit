;;;;
;;;; Protocol for the operationalizer
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus)

(defvar *debug-compile* nil
  "When true, prints extra debug information about the semantics compilation")

(defmacro when-debug-compile (&body body)
  "Run BODY when *DEBUG-COMPILE* is set"
  `(when *debug-compile* ,@body))

(defmacro if-debug-compile (then &optional else)
  "Run THEN when *DEBUG-COMPILE* is set, otherwise run ELSE"
  (if else
      `(if *debug-compile* ,then ,else)
      `(if *debug-compile* ,then)))

(defgeneric operationalize-chc (chc smt-context semgus-context)
  (:documentation "Operationalizes a CHC with the given SMT and SemGuS contexts.
Returns a calling card object as the result."))

(defmethod operationalize-chc :around (chc smt-context semgus-context)
  "If debug compile enabled, reports timing data about compiling CHC"
  (if-debug-compile
   (let ((chc-string (format nil "~a" (chc:name (chc:constructor chc)))))
     (format *trace-output* "~&; Compiling CHC: ~a~%" chc-string)
     (u:with-timing (time)
       (prog1
           (call-next-method)
         (format *trace-output* "~&; Compiled [~,2fs]: ~a~%" time chc-string))))
   (call-next-method)))
