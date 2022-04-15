;;;
;;; com.kjcjohnson.synthkit system definition
;;;
(asdf:defsystem "com.kjcjohnson.synthkit"
  :description "Program synthesis toolkit for Common Lisp"
  :version "0.0.1"
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "TBD"
  :depends-on ("cl-smt-lib"
               "closer-mop"
               "com.kjcjohnson.kale")
  :components ((:file "package")
               (:file "utilities" :depends-on ("package"))
               (:file "smt" :depends-on ("package"))
               (:file "grammar" :depends-on ("package" "utilities"))
               (:file "ast" :depends-on ("package" "grammar" "smt"))
               (:module "semgus"
                :depends-on ("package" "grammar" "ast" "smt" "utilities")
                :components ((:file "semgus")
                             (:file "semantics")
                             (:file "cegis" :depends-on ("semantics"))))
               (:module "vsa"
                :depends-on ("package" "ast" "grammar")
                :components ((:file "program-node")
                             (:file "empty-program-node"
                              :depends-on ("program-node"))
                             (:file "cross-program-node"
                              :depends-on ("program-node"))
                             (:file "union-program-node"
                              :depends-on ("program-node"))
                             (:file "leaf-program-node"
                              :depends-on ("program-node"))))))

(asdf:defsystem "com.kjcjohnson.synthkit/semgus/interop"
  :description "Iterop utilities for SemGuS and kl-synthkit"
  :version "0.0.1"
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "TBD"
  :depends-on ("com.kjcjohnson.synthkit" "bike")
  :components ((:module "semgus"
                :components ((:module "interop"
                              :components ((:file "package")
                                           (:file "interop")))))))
