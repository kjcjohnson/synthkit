;;;
;;; com.kjcjohnson.synthkit system definition
;;;
(asdf:defsystem "com.kjcjohnson.synthkit"
  :description "Program synthesis toolkit for Common Lisp"
  :version "0.0.1"
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "TBD"
  :depends-on ("cl-smt-lib")
  :components ((:file "package")
               (:file "utilities" :depends-on ("package"))
               (:file "smt" :depends-on ("package"))
               (:file "grammar" :depends-on ("package" "utilities"))
               (:file "ast" :depends-on ("package" "grammar" "smt"))
               (:file "semgus" :depends-on ("package" "grammar" "ast" "smt"))))
