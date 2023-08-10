;;;;
;;;; SMT package
;;;;
(defpackage #:com.kjcjohnson.synthkit.smt
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:* #:serapeum/bundle)
                    (#:? #:trivia))
  (:shadow #:sort #:variable)
  (:export #:name #:children #:to-smt #:definition #:sort #:child-sorts #:arity
           #:*int-sort* #:*bool-sort* #:*string-sort* #:variable
           #:$int #:$bool #:$string #:$function #:$literal
           #:*reglan-sort*
           #:$exists #:$forall #:$true #:$false #:$apply
           #:$+ #:$- #:$< #:$> #:$* #:$=
           #:$not #:$and #:$or #:$xor
           #:$iff #:$implies #:$ite
           #:with-solver #:make-solver #:close-solver
           #:check-sat #:read-model #:get-model
           #:push-scope #:pop-scope #:with-scope
           #:add #:declare-constants #:dump-commands #:set-model
           #:copy-node

           #:*smt* #:init-smt
           #:ensure-identifier #:ensure-sort #:identifier-string #:identifier-smt

           #:get-function-definition #:set-function-definition
           #:get-compiled-function #:evaluate-expression

           #:get-sort
           #:datatype #:datatype-constructor #:constructors
           #:add-datatype-constructor
           #:datatype=
           #:is-datatype-instance?

           #:state #:make-state #:copy-state #:get-value #:state= #:state-hash-code
           #:evaluate-state #:make-temp-state #:canonicalize-state
           #:get-first-value #:get-variables

           #:get-constant-type

           #:is-application?

           #:defsmtfun
           #:do-call-smt #:call-smt)

  ;; Natives
  (:export #:make-native-literal #:native-literal? #:native-value
           #:make-native-break #:native-break? #:native-break-condition)

  ;; Solvers
  (:export #:*solver* #:program #:arguments
           #:with-solver #:with-solver* #:with-lazy-solver
           #:make-solver #:initialize-solver #:finalize-solver #:cleanup-solver
           #:solver #:solver* #:*cvc5* #:arguments #:add-assertion
           #:declare-constant #:dump #:reset-solver #:set-logic
           #:solve)

  ;; Traversals
  (:export #:map-expression #:update-expression)

  ;; Trivia patterns
  (:export #:application #:fn #:quantifier #:forall #:exists #:var
           #:sort #:bool-sort #:int-sort #:string-sort #:reglan-sort))
