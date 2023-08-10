;;;;
;;;; Extensions to Trivia patterns for matching on SMT objects
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(?:defpattern application (name children &key (context '*smt*))
  "Matches against an SMT function application. Name should be passed as a string,
or _ to match against any function. Also aliased to FN."
  ;; Support matching on either all children (as a list) or individual children
  (when (listp children)
    (setf children `(list ,@children)))
  ;; Auto-convert to SMT names via the context
  (if (constantp name)
      (a:with-gensyms (name-var)
        `(?:guard (expression :name ,name-var :children ,children)
                  (eql ,name-var (ensure-identifier ,name ,context))))
      `(expression :name ,name :children ,children)))

(?:defpattern fn (name children &key (context '*smt*))
  "Matches against an SMT function application. Name should be passed as a string,
or _ to match against any function. Also aliased to APPLICATION."
  `(application ,name ,children :context ,context))

(?:defpattern quantifier (type bindings body &key (context '*smt*))
  "Matches against an SMT quantifier. TYPE is either :EXISTS or :FORALL. BINDINGS can
be either a single variable to capture everything, or a list for each separately."
  (declare (type (member :exists :forall) type))
  (declare (ignore context))
  ;; Support both forms of bindings
  (when (listp bindings)
    (setf bindings `(list ,@bindings)))
  `(class quantifier :name ,(ecase type (:exists "exists") (:forall "forall"))
                     :arguments ,bindings
                     :children (list ,body)))

(?:defpattern forall (bindings body &key (context '*smt*))
  "Matches against a universal quantifier. BINDINGS can either be a single variable to
capture all bindings, or a list to capture each binding individually."
  `(quantifier :forall ,bindings ,body :context ,context))

(?:defpattern exists (bindings body &key (context '*smt*))
  "Matches against an existential quantifier. BINDINGS can either be a single variable
to capture all bindings, or a list to capture each binding individually."
  `(quantifier :exists ,bindings ,body :context ,context))

(?:defpattern var (name &key sort (context '*smt*))
  "Matches against an variable."
  (declare (ignore context))
  `(class constant :name ,name :sort ,(if (null sort) '_ sort)))

(?:defpattern sort (name &key (context '*smt*))
  "Matches against a sort."
  (declare (ignore context))
  `(class sort :name ,name))

(?:defpattern bool-sort ()
  "Matches Boolean sorts"
  `(sort "Bool"))

(?:defpattern int-sort ()
  "Matches Integer sorts"
  `(sort "Int"))

(?:defpattern string-sort ()
  "Matches String sorts"
  `(sort "String"))

(?:defpattern reglan-sort ()
  "Matches Reglan sorts"
  `(sort "RegLan"))
