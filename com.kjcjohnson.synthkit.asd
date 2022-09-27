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
               "str"
               "bit-smasher"
               "trivial-garbage"
               "com.kjcjohnson.kale")
  :components ((:file "package")
               (:file "utilities" :depends-on ("package"))
               (:module "smt"
                :depends-on ("package")
                :serial t
                :components ((:file "smt")
                             (:file "types")
                             (:file "theory-core")
                             (:file "theory-ints")
                             (:file "theory-strings")
                             (:file "theory-bitvectors")
                             (:file "theory-dispatch")
                             (:file "datatypes")
                             (:file "context")
                             (:file "compiler")
                             (:file "evaluator")
                             (:file "predicates")
                             (:file "states")))
               (:module "grammar"
                :depends-on ("package" "utilities")
                :serial t
                :components ((:file "grammar")
                             (:file "distance-to-leaves")))
               (:file "ast" :depends-on ("package" "grammar" "smt"))
               (:module "semgus"
                :depends-on ("package" "grammar" "ast" "smt" "utilities")
                :serial t
                :components ((:file "semgus")
                             (:file "semantics")
                             (:file "reader")
                             (:file "cegis")))
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
                              :depends-on ("program-node"))
                             (:file "utilities"
                              :depends-on ("program-node"
                                           "union-program-node"
                                           "union-program-node"))))))
