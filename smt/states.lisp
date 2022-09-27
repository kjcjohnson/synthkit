;;;;
;;;; SMT states - mappings from variable IDs to values
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defclass state ()
  ((mapping :initarg :mapping
            :reader mapping)
   (canonical :initarg :canonical
              :reader is-canonical-state?))
  (:default-initargs :mapping nil :canonical nil))

(defmethod print-object ((state state) stream)
  "Prints a representation of the STATE to the STREAM."
  (print-unreadable-object (state stream :type t)
    (loop for (var . value) in (mapping state)
          doing (format stream " ~a: ~a " var value))))

;;;
;;; Hash-consing
;;;
;;; States a hash-consed, so we only have a single canonical instance for each
;;; possible state value. This makes comparisons fast and avoids duplication.
;;;
(defparameter *canonical-state-instances*
  (trivial-garbage:make-weak-hash-table :weakness nil :test #'equal))

(defun %normalize-state-mapping! (alist-mapping)
  "Deduplicates and sorts the mapping to a canonical ordering. Destructive."
  (cl:sort (delete-duplicates alist-mapping
                              :from-end t ; preserve alist semantics
                              :key #'car)
           #'string<
           :key #'(lambda (x) (symbol-name (car x)))))

(defun %state-arguments-to-alist (args)
  "Converts argument list into an alist. Always returns a fresh list."
  (when (= 1 (length args))
    (setf args (first args)))

  (let (arglist)
    (if (consp (first args))
        (setf arglist (copy-list args)) ; Assume already in alist form
        (setf arglist (loop for (var value) on args by #'cddr
                            collecting (cons var value)))) ; Plist-to-alist
    arglist))

(defparameter *total-state-usage-count* 0)
(defparameter *total-state-alloc-count* 0)

(defun %print-state-alloc-stats ()
  "Prints statistics about state allocations to *TRACE-OUTPUT*."
  (format *trace-output*
          "; State Alloc Stats: ~a [Alloc] ~a [Used] ~a [Retained] ~,2f% ~,2f%~%"
          *total-state-usage-count*
          *total-state-alloc-count*
          (hash-table-count *canonical-state-instances*)
          (* 100
             (/ *total-state-alloc-count*
                *total-state-usage-count*))
          (* 100
             (/ (hash-table-count *canonical-state-instances*)
                *total-state-usage-count*))))

(defun make-temp-state (&rest args)
  "Creates a throwaway, non-canonical state."
  (make-instance 'state
                 :mapping (%state-arguments-to-alist args)
                 :canonical nil))

(defun canonicalize-state (state)
  "Returns a canonical version of the given state."
  (if (is-canonical-state? state)
      state
      (make-state (mapping state))))

(defun make-state (&rest args)
  "Creates a state with the given mappings, specified either as:
 - pairs of VAR VALUE (plist-style)
 - cons of (VAR . VALUE) (alist-style)
 - if a single-element list is given, then that element is treated as the list"
  (let* ((arglist (%normalize-state-mapping! (%state-arguments-to-alist args)))
         (canonical-instance (gethash arglist *canonical-state-instances*)))
    (incf *total-state-usage-count*)
    (when (null canonical-instance)
      (incf *total-state-alloc-count*)
      (setf canonical-instance (make-instance 'state
                                              :mapping arglist
                                              :canonical t))
      (setf (gethash arglist *canonical-state-instances*) canonical-instance))

    '(when (zerop (rem *total-state-usage-count* 100000))
      (%print-state-alloc-stats))
    canonical-instance))

(defun get-value (state var)
  "Gets a value from the given state."
  (declare (type state state))
  (let ((pair (assoc var (mapping state))))
    (values (cdr pair) (not (null pair)))))

(defun copy-state (state &rest modifications)
  "Copies a state and optionally replaces variables, specified in pairs:VAR VALUE."
  (declare (type state state))
  (make-state (nconc (%state-arguments-to-alist modifications)
                     (mapping state)))) ; My understanding is that the last arg
                                        ; doesn't get modified by nconc.

(defun evaluate-state (state &optional (ctx *smt*))
  "Returns a new state with all mappings in STATE evaluated under the context CTX."
  (make-state (loop for (var . value) in (mapping state)
                    collecting var
                    collecting (evaluate-expression value ctx))))

(defun state= (state1 state2)
  "Checks two states for equality. Both states must have identical variables."
  (declare (type state state1 state2))

  ;;; Short-circuit if both states are canonical instances
  (when (and (is-canonical-state? state1)
             (is-canonical-state? state2))
    (return-from state= (eql state1 state2)))

  (warn "Comparing non-canonical states")
  ;;; Otherwise, check if the keys are same
  (when (set-exclusive-or (mapping state1) (mapping state2) :key #'car)
    (return-from state= nil))

  ;;; If the keys are the same, compare the values
  (loop for (var . value1) in (mapping state1)
        for value2 = (get-value state2 var)
        unless (and (eql (get-constant-type value1)
                         (get-constant-type value2))
                    (core-= value1 value2))
          do (return-from state= nil))
  t)

(defmethod com.kjcjohnson.kale:equals ((s1 state) (s2 state))
  (state= s1 s2))

(defmethod com.kjcjohnson.kale:get-hash-code ((s state))
  "Computes a hash code for the state S."
  (sxhash (mapping s)))

