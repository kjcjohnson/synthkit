;;;;
;;;; Symbols and symbol tables
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

;;
;; Symbol tables
;;
(defun com.kjcjohnson.synthkit.semgus.reader.user::symbol-entry
    (name &key sort index)
  "A symbol entry"
  (make-instance 'chc:symbol-entry :name name :sort sort :index index))

(defun com.kjcjohnson.synthkit.semgus.reader.user::symbol-table
    (&key inputs outputs term auxiliary children)
  "A CHC's symbol table"
  (make-instance 'chc:symbol-table
                 :inputs (u:ensure-vector inputs)
                 :outputs (u:ensure-vector outputs)
                 :term term
                 :auxiliary (u:ensure-vector auxiliary)
                 :children (u:ensure-vector children)))

;;;
;;; Handling for creating symbol table instances from the legacy format
;;;
(defun %try-derive-aux-sort (variable body constraint)
  "Attempts to find the sort of an auxiliary variable. Signals a warning and returns NIL
if unable to derive a sort, which can happen if a variable is not used anywhere."
  (declare (ignore constraint))

  ;; Look in body relations first
  (dolist (br body)
    (a:when-let (pos (position variable (chc:actuals br)))
      (return-from %try-derive-aux-sort (elt (chc:signature br) pos))))

  ;; Then traverse the constraint...if needed. But chances are we don't need to.
  ;; TODO if we care
  (warn "Unable to derive sort for auxiliary variable: ~a" variable)
  nil)

(defun convert-legacy-symbol-table
    (&key head body constraint input-variables output-variables variables constructor)
  "Attempts to create a SYMBOL-TABLE from the loose variables in the previous format."

  (let ((head-sig (chc:signature head))
        (head-var (chc:actuals head))
        (term nil)
        (inputs (make-array (length input-variables)))
        (outputs (make-array (length output-variables)))
        (auxiliary nil)
        (children (make-array (length (chc:arguments constructor)))))

    ;; Term - assumed to be index 0 of the CHC head
    (setf term (make-instance 'chc:symbol-entry
                              :name (elt head-var 0)
                              :sort (elt head-sig 0)
                              :index 0))

    ;; Input variables
    (dotimes (i (length input-variables))
      (let ((sig-pos (position (nth i input-variables) head-var)))
        (setf (aref inputs i) (make-instance 'chc:symbol-entry
                                             :name (elt head-var sig-pos)
                                             :sort (elt head-sig sig-pos)
                                             :index sig-pos))))

    ;; Output variables
    (dotimes (i (length output-variables))
      (let ((sig-pos (position (nth i output-variables) head-var)))
        (setf (aref outputs i) (make-instance 'chc:symbol-entry
                                              :name (elt head-var sig-pos)
                                              :sort (elt head-sig sig-pos)
                                              :index sig-pos))))

    ;; Auxiliary variables
    ;;  - we can _probably_ figure out the sort, but it's not always possible. We would
    ;;   need to traverse the body relations and constraints and look for usages. Plus
    ;;   if a variable isn't used, we won't know what sort it is. So we just guess.
    (setf auxiliary
          (delete-if #'null
                     (map 'vector
                          #'(lambda (v)
                              (unless (or (find v input-variables)
                                          (find v output-variables)
                                          (eql v (chc:symbol-name term)))
                                (make-instance 'chc:symbol-entry
                                               :name v
                                               :sort (%try-derive-aux-sort
                                                      v
                                                      body
                                                      constraint)
                                               :index nil)))
                          variables)))

    ;; Children
    (dotimes (i (length (chc:arguments constructor)))
      (setf (aref children i)
            (make-instance 'chc:symbol-entry
                           :name (elt (chc:arguments constructor) i)
                           :sort (elt (chc:argument-sorts constructor) i)
                           :index i)))

    ;; Create and return the table!
    (make-instance 'chc:symbol-table
                   :term term
                   :inputs inputs
                   :outputs outputs
                   :auxiliary auxiliary
                   :children children)))
