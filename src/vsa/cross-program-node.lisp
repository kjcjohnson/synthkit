(in-package #:com.kjcjohnson.synthkit.vsa)

;;;
;;; Cartesian product enumerator
;;;
(defclass cross-program-node-enumerator (program-node-enumerator)
  ((started :accessor %started
            :initarg :started)
   (production :accessor %production
               :initarg :production)
   (enumerators :accessor %enumerators
                :initarg :enumerators))
  (:default-initargs :started nil))

(defmethod initialize-instance :after ((obj cross-program-node-enumerator) &key sets)
  "Sets up the cross program enumerator"
  (setf (%enumerators obj) (map 'list #'enumerator sets)))

(defun cross-program-node-reset (e)
  (setf (%started e) nil))

(defun cross-program-node-advance (e)
  (labels ((adv (enlist)
             (unless (endp enlist)
               (let ((res (move-next (first enlist))))
                 (if (null res)
                     (progn
                       (reset (first enlist))
                       (move-next (first enlist))
                       (adv (rest enlist)))
                     t)))))
    (adv (%enumerators e))))

(defun cross-program-node-restart (e)
  (setf (%started e) t)
  (flet ((restart-enum (enum)
           (reset enum)
           (move-next enum)))
    (every #'restart-enum (%enumerators e))))

(defun cross-program-node-move-next (e)
  (if (%started e)
      (cross-program-node-advance e)
      (cross-program-node-restart e)))

(defun cross-program-node-current (e)
  (make-instance
   'ast:program-node
   :production (%production e)
   :children (map 'list
                  #'(lambda (enum)
                      (current enum))
                  (%enumerators e))))

;;;
;;; Cartesian product of program nodes
;;;
(defclass cross-program-node (program-node)
  ((production :reader %production
               :initarg :production
               :type g:production
               :documentation "Prodcution associated with this node")
   (sets :reader %sets
         :initarg :sets
         :type list
         :documentation "List of sets of program nodes for each child"))
  (:documentation "A program node that takes the Cartesian product of each set"))

(defmethod initialize-instance :after ((node cross-program-node) &key)
  "Verifies that the cross program node is configured properly"
  (assert (= (g:arity (%production node)) (length (%sets node)))))

(defun is-cross-program-node? (node)
  "Checks if NODE is a cross program node"
  (typep node 'cross-program-node))

(defmethod program-count ((node cross-program-node))
  "Get the number of programs rooted at NODE. For cross program nodes, this is the
product of the count of all child sets."
  (reduce #'* (%sets node)
          :key #'program-count
          :initial-value 1))

(defmethod enumerator ((node cross-program-node))
  "Gets an enumerator over the cross product of this node's children"
  (make-instance 'cross-program-node-enumerator
                 :production (%production node)
                 :sets (%sets node)
                 :reset-fn #'cross-program-node-reset
                 :move-next-fn #'cross-program-node-move-next
                 :current-fn #'cross-program-node-current))
