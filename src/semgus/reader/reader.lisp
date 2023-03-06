;;;;
;;;; The main reader interface
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

(defun find-user-package ()
  "Finds the reader user package"
  (find-package "COM.KJCJOHNSON.SYNTHKIT.SEMGUS.READER.USER"))

(defmethod semgus:read-problem-from-stream (stream context)
  "Reads a SemGuS problem from STREAM into CONTEXT"
  (let ((semgus:*semgus-context* context))
    (loop for sexpr = (let ((*package* (find-user-package)))
                        (read stream nil :eof))
          until (eql sexpr :eof)
          doing (eval sexpr))
    context))

(defmethod semgus:read-problem-from-stream :after (stream context)
  "Post-read actions"
  ;; We need to fixup forward references in CHCs
  (dolist (chc (semgus:chcs context))
    (dolist (br (chc:body chc))
      (let ((head (chc:head br)))
        (when (chc:is-forward-declared-head? head)
          (chc:fixup-forward-declared-head br (semgus:lookup-head (chc:name head)
                                                                  context)))))))
