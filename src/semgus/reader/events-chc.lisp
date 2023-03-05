;;;;
;;;; CHC events
;;;;
(in-package #:com.kjcjohnson.synthkit.semgus.reader)

(defun com.kjcjohnson.synthkit.semgus.reader.user::relation
    (name &key signature arguments)
  "Creates a relation"
  (make-instance 'semgus::semgus-relation
                 :name name
                 :signature signature
                 :arguments arguments))

(defun com.kjcjohnson.synthkit.semgus.reader.user::constructor
    (name &key arguments argument-sorts return-sort)
  "Creates a constructor"
  (make-instance 'semgus::semgus-chc-constructor
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
   (semgus:chcs semgus:*semgus-context*))
  (unless (find (semgus:name head)
                (semgus:head-relations semgus:*semgus-context*)
                :test #'(lambda (n r)
                          (eql n (semgus:name r))))
    (let ((term-index 0) ;; Shouldn't hardcode this - TODO
          (input-indexes (loop for i from 0
                                         upto (length (semgus::signature head))
                                         for arg = (nth i (semgus:arguments head))
                                         when (find arg input-variables)
                                           collect i))
          (output-indexes (loop for i from 0
                                         upto (length (semgus::signature head))
                                         for arg = (nth i (semgus:arguments head))
                                          when (find arg output-variables)
                                           collect i)))

      (push
       (make-instance 'semgus::semgus-chc-head
                      :name (semgus:name head)
                      :term-index term-index
                      :term-type (nth term-index (semgus::signature head))
                      :argument-sorts (semgus::signature head)
                      :input-indexes input-indexes
                      :output-indexes output-indexes
                      :term-name (nth term-index (semgus:arguments head))
                      :input-names (map 'list
                                        #'(lambda (i) (nth i (semgus:arguments head)))
                                        input-indexes) ;; TOD: warn if no match
                      :output-names (map 'list
                                         #'(lambda (i) (nth i (semgus:arguments head)))
                                         output-indexes))
       (semgus:head-relations semgus:*semgus-context*)))))
