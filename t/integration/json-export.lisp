;;;;
;;;; JSON export test script
;;;;
(defpackage #:test.json-export
  (:use #:cl)
  (:local-nicknames (#:semgus #:com.kjcjohnson.synthkit.semgus))
  (:export #:main))

(in-package #:test.json-export)

(defun main()
  (dolist (file (uiop:command-line-arguments))
    (let ((problem (semgus:load-semgus-problem file)))
      (with-open-file (fs (str:concat file ".test.json")
                          :direction :output
                          :if-exists :supersede)
        (semgus:write-problem fs problem :json)))))
