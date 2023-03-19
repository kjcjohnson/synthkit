;;;
;;; com.kjcjohnson.synthkit system and sub-system definitions
;;;
(asdf:defsystem "com.kjcjohnson.synthkit"
  :description "Program synthesis toolkit for Common Lisp"
  :version (:read-file-form "version.sexpr")
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "MIT"
  :in-order-to ((test-op (test-op "com.kjcjohnson.synthkit/test")))
  :depends-on ("com.kjcjohnson.synthkit/base"
               "com.kjcjohnson.synthkit/smt"
               "com.kjcjohnson.synthkit/grammar"
               "com.kjcjohnson.synthkit/ast"
               "com.kjcjohnson.synthkit/specification"
               "com.kjcjohnson.synthkit/semgus"
               "com.kjcjohnson.synthkit/vsa"))

(asdf:defsystem "com.kjcjohnson.synthkit/base"
  :description "Base synthkit dependencies and utilities"
  :version (:read-file-form "version.sexpr")
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "MIT"
  :depends-on ("cl-smt-lib"
               "closer-mop"
               "str"
               "bit-smasher"
               "trivial-garbage"
               "trivia"
               "graph"
               "alexandria")
  :pathname "src"
  :components ((:file "package")
               (:file "utilities" :depends-on ("package"))))

(asdf:defsystem "com.kjcjohnson.synthkit/smt"
  :description "SMT-LIB2 objects and processing"
  :version (:read-file-form "version.sexpr")
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "MIT"
  :pathname "src/smt"
  :depends-on ("com.kjcjohnson.synthkit/base"
               "cl-smt-lib")
  :serial t
  :components ((:file "package")
               (:file "protocol-hash-code")
               (:file "solver")
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

(asdf:defsystem "com.kjcjohnson.synthkit/grammar"
  :description "Regular tree grammars"
  :version (:read-file-form "version.sexpr")
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "MIT"
  :pathname "src/grammar"
  :depends-on ("com.kjcjohnson.synthkit/base")
  :serial t
  :components ((:file "package")
               (:file "grammar")
               (:file "distance-to-leaves")))

(asdf:defsystem "com.kjcjohnson.synthkit/ast"
  :description "Abstract syntax trees and execution"
  :version (:read-file-form "version.sexpr")
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "MIT"
  :pathname "src/ast"
  :depends-on ("com.kjcjohnson.synthkit/base"
               "com.kjcjohnson.synthkit/grammar"
               "com.kjcjohnson.synthkit/smt")
  :serial t
  :components ((:file "package")
               (:file "atom")
               (:file "node")
               (:file "hole")
               (:file "ast")
               (:file "calling-card")
               (:file "execution")))

(asdf:defsystem "com.kjcjohnson.synthkit/specification"
  :description "Synthesis problem specifications"
  :version (:read-file-form "version.sexpr")
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "MIT"
  :pathname "src/specification"
  :depends-on ("com.kjcjohnson.synthkit/base"
               "com.kjcjohnson.synthkit/smt")
  :serial t
  :components ((:file "package")
               (:file "base")
               (:file "joiners")
               (:file "inductive")
               (:file "cegis")))

(asdf:defsystem "com.kjcjohnson.synthkit/semgus"
  :description "SemGuS support in synthkit"
  :version (:read-file-form "version.sexpr")
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "MIT"
  :pathname "src/semgus"
  :depends-on ("com.kjcjohnson.synthkit/base"
               "com.kjcjohnson.synthkit/grammar"
               "com.kjcjohnson.synthkit/ast"
               "com.kjcjohnson.synthkit/smt")
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

(asdf:defsystem "com.kjcjohnson.synthkit/vsa"
  :description "Version space algebra"
  :version (:read-file-form "version.sexpr")
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "MIT"
  :pathname "src/vsa"
  :depends-on ("com.kjcjohnson.synthkit/base")
  :serial t
  :components ((:file "package")
               (:file "program-node")
               (:file "enumerator")
               (:file "empty-program-node")
               (:file "cross-program-node")
               (:file "union-program-node")
               (:file "leaf-program-node")
               (:file "utilities"
                :depends-on ("program-node"
                             "union-program-node"
                             "union-program-node"))))

(asdf:defsystem "com.kjcjohnson.synthkit/test"
  :description "Tests for synthkit"
  :version (:read-file-form "version.sexpr")
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "MIT"
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
                                           (:file "regex")))))
               (:module "vsa"
                :serial t
                :components ((:file "main")
                             (:file "empty-program-node")
                             (:file "leaf-program-node")
                             (:file "union-program-node")
                             (:file "cross-program-node")))))
