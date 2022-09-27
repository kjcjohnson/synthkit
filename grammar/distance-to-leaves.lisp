;;;;
;;;; Distance to Leaves - a grammar computation
;;;;
;;;; Assigns a value to each production being the distance to a leaf.
;;;; Each leaf production gets a value of 0, and minimum distance to
;;;; leaf otherwise.
(in-package #:com.kjcjohnson.synthkit.grammar)

(defun %initialize-distance-to-leaves-table (grammar)
  "Creates and initializes the distance table. All elements are initialized
to MOST-POSITIVE-FIXNUM."
  (let ((table (make-hash-table)))
    (dolist (prod (productions grammar))
      (setf (gethash prod table) most-positive-fixnum))
    (dolist (nt (non-terminals grammar))
      (setf (gethash nt table) most-positive-fixnum))
    table))

(defun %compute-distance-to-leaves-1 (grammar table)
  "Computes a single step of the distance-to-leaves extra data."
  (let ((changed? nil))
    (flet ((compare-and-set (elt value)
             "Convenience function to set a value and update the changed flag."
             (multiple-value-bind (current found?)
                 (gethash elt table)
               (setf changed? (or changed? (not found?) (not (= current value)))))
             (setf (gethash elt table) value)))

      (dolist (prod (productions grammar))
        (if (zerop (arity prod))
            (compare-and-set prod 0)
            (compare-and-set prod (1+ (reduce #'min
                                              (occurrences prod)
                                              :key #'(lambda (nt)
                                                       (gethash nt table)))))))
      (dolist (nt (non-terminals grammar))
        (compare-and-set nt (reduce #'min
                                    (productions-for-instance grammar nt)
                                    :key #'(lambda (prod)
                                             (gethash prod table))))))
    changed?))

(defun %compute-distance-to-leaves (grammar table)
  "Computes the distance-to-leaves fixpoint."
  ;; Run until a fixpointx
  (loop while (%compute-distance-to-leaves-1 grammar table)))

(defun compute-distance-to-leaves (grammar)
  "Computes the distance-to-leaves transform."
  (let ((table (%initialize-distance-to-leaves-table grammar)))
    (%compute-distance-to-leaves grammar table)
    table))

(defun distance-to-leaves (grammar elt)
  "Gets the distance from a grammar element to the closest leaf."
  (let ((table (extra-data grammar 'distance-to-leaves)))
    (when (null table)
      (setf table (compute-distance-to-leaves grammar))
      (setf (extra-data grammar 'distance-to-leaves) table))
    (multiple-value-bind (distance found?)
        (gethash elt table)
      (unless found? (error "Element not in grammar ~a: ~a" grammar elt))
      distance)))
