;;;
;;; com.kjcjohnson.synthkit system definition
;;;
(asdf:defsystem "com.kjcjohnson.synthkit"
  :description "Program synthesis toolkit for Common Lisp"
  :version "0.0.1"
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "TBD"
  :in-order-to ((test-op (test-op "com.kjcjohnson.synthkit/test")))
  :depends-on ("cl-smt-lib"
               "closer-mop"
               "str"
               "bit-smasher"
               "trivial-garbage"
               "trivia"
               "graph"
               "alexandria"
               "com.kjcjohnson.kale")
  :pathname "src"
  :components ((:file "package")
               (:file "utilities" :depends-on ("package"))
               (:module "smt"
                :depends-on ()
                :serial t
                :components ((:file "package")
                             (:file "smt")
                             (:file "theory-dispatch")
                             (:file "theory-ints")
                             (:file "theory-bitvectors")
                             (:file "theory-strings")
                             (:file "datatypes")
                             (:file "types")
                             (:file "theory-core")
                             (:file "context")
                             (:file "compiler")
                             (:file "evaluator")
                             (:file "predicates")
                             (:file "states")
                             (:file "traversal")))
               (:module "grammar"
                :depends-on ("utilities")
                :serial t
                :components ((:file "package")
                             (:file "grammar")
                             (:file "distance-to-leaves")))
               (:module "ast"
                :depends-on ("grammar" "smt")
                :serial t
                :components ((:file "package")
                             (:file "atom")
                             (:file "node")
                             (:file "hole")
                             (:file "ast")
                             (:file "calling-card")
                             (:file "execution")))
               (:module "specification"
                :depends-on ("smt")
                :serial t
                :components ((:file "package")
                             (:file "base")
                             (:file "joiners")
                             (:file "inductive")
                             (:file "cegis")))
               (:module "semgus"
                :depends-on ("grammar" "ast" "smt" "utilities")
                :serial t
                :components ((:module "chc"
                              :serial t
                              :components ((:file "package")
                                           (:file "chc")
                                           (:file "symbol-table")
                                           (:file "operations")))
                             (:file "package")
                             (:file "context")
                             (:file "semgus")
                             (:file "protocol-reader")
                             (:file "semantics")
                             (:module "operationalizer"
                              :serial t
                              :components ((:file "package")
                                           (:file "expressions")
                                           (:file "codegen")
                                           (:file "operationalizer")))
                             (:file "reader_mono")
                             (:module "reader"
                              :serial t
                              :components ((:file "package")
                                           (:file "events-chc")
                                           (:file "events-datatypes")
                                           (:file "events-functions")
                                           (:file "events-meta")
                                           (:file "events-symbol-table")
                                           (:file "events-synthesis")
                                           (:file "events-terms")
                                           (:file "events-utility")
                                           (:file "constraints")
                                           (:file "semantics")
                                           (:file "reader")))
                             (:file "verifier")
                             (:module "verifiers"
                              :serial t
                              :components ((:file "package")
                                           (:file "glue")
                                           (:file "concretizing")
                                           (:file "operational")))
                             (:file "cegis")))
               (:module "vsa"
                :depends-on ("ast" "grammar")
                :components ((:file "package")
                             (:file "program-node")
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

(asdf:defsystem "com.kjcjohnson.synthkit/test"
  :description "Tests for synthkit"
  :version "0.0.1"
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "TBD"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run! :synthkit-tests))
  :depends-on ("fiveam"
               "com.kjcjohnson.synthkit")
  :pathname "t"
  :serial "t"
  :components ((:file "package")
               (:file "main")
               (:module "smt"
                :serial t
                :components ((:file "main")
                             (:module "theories"
                              :serial t
                              :components ((:file "main")
                                           (:file "strings")
                                           (:file "regex")))))))
