;;;
;;; .NET Interop for calling into the Prose SDK
;;;
(in-package :com.kjcjohnson.synthkit.semgus.interop)

;
; BIKE NOTE: for indexers, use (bike:ref target index ...indices)
;

(defparameter *dll-pathname* "semgus/interop/SemgusInterop/bin/Debug/net6.0/Semgus.Parser.Interop.CommonLisp.dll")

(defun init-interop ()
  "Initializes the .NET interoperability"
  (bike:import-assembly 
   (bike:load-assembly-from 
    (asdf:system-relative-pathname "com.kjcjohnson.synthkit"
                                   *dll-pathname*))))

(init-interop)

(bike:use-namespace "Semgus.Parser.Interop.CommonLisp")

(defun ilist-to-lisp-list (ilist &optional (conversion-fn #'identity))
  (let ((count (bike:property ilist 'Count)))
    (loop for i from 0 below count collecting (funcall conversion-fn (bike:ref ilist i)))))

(defun read-problem-file (filename)
  (let ((handler (bike:new 'InteropHandler)))
    (if (bike:invoke handler 'ParseSynthesisProblem filename)
        (format t "Successfully parsed.")
        (format t "Failed to parse."))
    (bike:property handler 'SemgusContexts)))

(defun convert-nt (cs-nt)
  "Converts a C# non-terminal to a Lisp non-terminal."
  (bike:property
   (bike:property cs-nt 'Name)
   'Symbol))

(defun convert-prod (cs-prod)
  "Converts a C# production to a Lisp production."
  (let* ((opname (bike:property
                  (bike:property
                   (bike:property cs-prod 'Constructor)
                   'Operator)
                  'Symbol))
         (name (concatenate 'string
               (convert-nt (bike:property cs-prod 'Instance))
               "->"
               opname)))
    `(
      ,name
      ,(convert-nt (bike:property cs-prod 'Instance))
      ,opname
     ,@(ilist-to-lisp-list (bike:property cs-prod 'Occurrences) #'convert-nt))))
   

(defun grammar-to-rtg (grammar)
  (let ((nts (ilist-to-lisp-list (bike:property grammar 'NonTerminals) #'convert-nt))
        (prs (ilist-to-lisp-list (bike:property grammar 'Productions) #'convert-prod)))
    (g:make-rtg
     :non-terminals nts
     :productions prs)))

(defun parse-problem-file (filename)
  (let ((contexts (read-problem-file (namestring filename))))
    (when (null contexts)
      (error "Error parsing problem file"))
    (when (zerop (bike:property contexts 'Count))
      (error "No contexts found in problem file"))
    (let* ((context (bike:ref contexts 0))
           (synthfuns (bike:property context 'SynthFuns)))
      (when (zerop (bike:property synthfuns 'Count))
        (error "No synthfuns found in problem file"))
      (let ((synthfun (bike:ref synthfuns 0)))
        (values (grammar-to-rtg (bike:property synthfun 'Grammar)))))))
      
