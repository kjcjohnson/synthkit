;;;;
;;;; SMT package
;;;;
(defpackage #:com.kjcjohnson.synthkit.smt
  (:use #:cl)
  (:local-nicknames (#:u #:com.kjcjohnson.synthkit.utilities)
                    (#:a #:alexandria)
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

           #:term #:application #:constant #:quantifier #:literal
           #:lambda-binder #:body
           #:recursive-declaration-grouper #:make-rec-group #:group #:group-type
           #:annotations #:add-annotation

           #:*smt* #:init-smt
           #:ensure-identifier #:ensure-sort #:identifier-string #:identifier-smt
           #:unique-identifier

           #:get-function-definition #:set-function-definition
           #:get-compiled-function #:evaluate-expression

           #:get-sort #:sort-parameters
           #:datatype #:datatype-constructor #:constructors
           #:add-datatype-constructor #:lookup-datatype-constructor
           #:datatype=
           #:is-datatype? #:is-datatype-instance?

           #:state #:make-state #:copy-state #:get-value #:state= #:state-hash-code
           #:evaluate-state #:make-temp-state #:canonicalize-state
           #:get-first-value #:get-variables

           #:get-constant-type

           #:rank #:argument-sorts #:return-sort #:make-rank

           #:is-application?

           #:defsmtfun
           #:do-call-smt #:call-smt)

  ;; Match
  (:export #:make-match-grouper #:make-match-binder
           #:match-grouper #:match-binder #:match-pattern
           #:match-pattern-singleton #:match-pattern-variable
           #:match-child #:match-binders #:match-pattern #:match-term
           #:match-pattern-datatype #:match-pattern-variables
           #:match-pattern-constructor #:match-pattern-datatype)

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
           #:sort #:bool-sort #:int-sort #:string-sort #:reglan-sort #:bv-sort))
