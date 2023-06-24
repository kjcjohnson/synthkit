;;;;
;;;; Inductive specifications - predicates over input and output states
;;;;
(in-package #:com.kjcjohnson.synthkit.specification)

(defclass inductive-specification (specification)
  ((input-state :initarg :input-state
                :reader input-state
                :documentation "Concrete input state for this specification")
   (descriptor :initarg :descriptor
               :reader descriptor
               :documentation "Semantics descriptor for this specification")
   (predicate :initarg :predicate
              :reader predicate
              :documentation "Function over two arguments taking the input and output
state to test for satisfication. Returns T if satisfied, NIL otherwise."))
  (:documentation "A specification over a concrete input and any output states"))

(defclass io-specification (inductive-specification)
  ((output-state :initarg :output-state
                 :reader output-state
                 :documentation "Concrete output state for this specification"))
  (:default-initargs :predicate #'smt:state=)
  (:documentation "A specification over a concrete input and concrete output state"))

(defun is-only-inductive? (spec)
  "Checks if SPEC only includes inductive specifications"
  (is-only? spec 'inductive-specification))

(defun is-only-io? (spec)
  "Checks if SPEC only includes IO specifications"
  (is-only? spec 'io-specification))

(defun is-pbe? (spec)
  "Checks if SPEC is a programming-by-example specification. That is, SPEC only consists
of IO specification leaves and intersection joiners."
  (and (is-only-io? spec) (with-only-intersection? spec)))

(defun leaves (spec)
  "Gets the leaf specifications of SPEC."
  (let ((result nil)
        (proc-queue (list spec)))
    (loop for item = (pop proc-queue)
          until (null item)
          if (typep item 'compound-specification)
            do (setf proc-queue (append proc-queue (components item)))
          else
            do (push item result)
          end)
    result))

(defun examples (spec)
  "Gets a list of examples, but only if SPEC satisfies IS-PBE?"
  (assert (is-pbe? spec))
  (leaves spec))

(defun filter-examples (spec predicate &key key)
  "Creates a new PBE spec from SPEC with only examples satisfying PREDICATE"
  (make-instance 'intersection-specification
                 :components (remove-if-not predicate (leaves spec) :key key)))

(defun filter-examples-by-descriptor (spec &key include exclude)
  "Filters examples by descriptors"
  (setf include (a:ensure-list include))
  (setf exclude (a:ensure-list exclude))
  (flet ((predicate (descriptor)
           (and (member descriptor include)
                (not (member descriptor exclude)))))
    (filter-examples spec #'predicate :key #'descriptor)))
