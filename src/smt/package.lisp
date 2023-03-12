;;;;
;;;; SMT package
;;;;
(defpackage #:com.kjcjohnson.synthkit.smt
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:shadow #:sort #:variable)
  (:export #:name #:children #:to-smt #:definition #:sort #:child-sorts #:arity
           #:*int-sort* #:*bool-sort* #:*string-sort* #:variable
           #:$int #:$bool #:$string #:$function
           #:$exists #:$forall #:$true #:$false #:$apply
           #:$+ #:$- #:$< #:$> #:$* #:$=
           #:$not #:$and #:$or #:$xor
           #:$iff #:$implies #:$ite
           #:with-solver #:make-solver #:close-solver #:check-sat #:get-model
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

           #:state #:make-state #:copy-state #:get-value #:state=
           #:evaluate-state #:make-temp-state #:canonicalize-state
           #:get-first-value

           #:get-constant-type

           #:is-application?

           #:do-call-smt #:call-smt)

  ;; Traversals
  (:export #:map-expression #:update-expression))
