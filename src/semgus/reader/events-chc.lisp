;;;;
;;;; CHC events
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

(defun com.kjcjohnson.synthkit.semgus.reader.user::relation
    (name &key signature arguments)
  "Creates a relation"
  (let ((head (semgus:lookup-head name)))
    ;; If the head hasn't been encountered yet, make a forward reference
    (when (null head)
      (setf head (make-instance 'chc:forward-declared-head
                                :name name
                                :signature (u:ensure-vector signature))))

    (make-instance 'chc:relation
                   :head head
                   :actuals (u:ensure-vector arguments))))

(defun com.kjcjohnson.synthkit.semgus.reader.user::constructor
    (name &key arguments argument-sorts return-sort)
  "Creates a constructor"
  (make-instance 'chc:constructor
                 :name name
                 :arguments arguments
                 :argument-sorts argument-sorts
                 :return-sort return-sort))

(defun com.kjcjohnson.synthkit.semgus.reader.user::chc
    (&key head body constraint
       input-variables output-variables variables symbols
       constructor)
  "Creates a CHC"
  ;; We may need to fix up the CHC variables
  (unless symbols
    (setf symbols (convert-legacy-symbol-table
                   :head head
                   :body body
                   :constraint constraint
                   :input-variables input-variables
                   :output-variables output-variables
                   :variables variables
                   :constructor constructor)))

  ;; Check if we need to create a CHC head object
  ;;  At this point, HEAD is a chc:relation, not a chc:head
  (if (chc:is-forward-declared-head? (chc:head head))
      (let ((new-head (chc:make-head-from-relation head symbols)))
        (semgus:add-head new-head)
        (setf head new-head))
      (setf head (chc:head head)))

  (push
   (make-instance 'semgus::semgus-chc
                  :head head
                  :body body
                  :constraint constraint
                  :input-variables input-variables
                  :output-variables output-variables
                  :variables variables
                  :symbol-table symbols
                  :constructor constructor)
   (semgus:chcs semgus:*semgus-context*)))
