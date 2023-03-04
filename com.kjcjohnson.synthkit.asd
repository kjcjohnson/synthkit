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
  :components ((:file "package")
               (:file "utilities" :depends-on ("package"))
               (:module "smt"
                :depends-on ("package")
                :serial t
                :components ((:file "smt")
                             (:file "types")
                             (:file "theory-core")
                             (:file "theory-ints")
                             (:file "theory-bitvectors")
                             (:file "theory-dispatch")
                             (:file "theory-strings")
                             (:file "datatypes")
                             (:file "context")
                             (:file "compiler")
                             (:file "evaluator")
                             (:file "predicates")
                             (:file "states")
                             (:file "traversal")))
               (:module "grammar"
                :depends-on ("package" "utilities")
                :serial t
                :components ((:file "grammar")
                             (:file "distance-to-leaves")))
               (:module "ast"
                :depends-on ("package" "grammar" "smt")
                :serial t
                :components ((:file "atom")
                             (:file "node")
                             (:file "hole")
                             (:file "ast")
                             (:file "calling-card")
                             (:file "execution")))
               (:module "specification"
                :pathname "src/specification"
                :depends-on ("smt")
                :serial t
                :components ((:file "package")
                             (:file "base")
                             (:file "joiners")
                             (:file "inductive")
                             (:file "cegis")))
               (:module "semgus"
                :pathname "src/semgus"
                :depends-on ("package" "grammar" "ast" "smt" "utilities")
                :serial t
                :components ((:module "chc"
                              :serial t
                              :components ((:file "package")
                                           (:file "chc")
                                           (:file "symbol-table")))
                             (:file "package")
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
                                           (:file "constraints")
                                           (:file "semantics")))
                             (:file "verifier")
                             (:module "verifiers"
                              :serial t
                              :components (#+()(:file "concretizing")))
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
