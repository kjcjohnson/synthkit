;;;
;;; Various useful utilities
;;;
(in-package #:com.kjcjohnson.synthkit.utilities)

(defun choose-uniformly-at-random (&rest options)
  "Chooses and returns one of OPTIONS uniformly at random."
  (nth (random (length options)) options))

;; Lifted from: https://stackoverflow.com/a/15098741
(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
            (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

(defun ensure-vector (sequence)
  "Checks if SEQUENCE is a vector. If so, returns it, else copies into a fresh vector"
  (if (vectorp sequence)
      sequence
      (map 'vector #'identity sequence)))

(defun ensure-list (sequence)
  "Checks if SEQUENCE is a list. If so, returns it, else copies into a fresh list"
  (if (listp sequence)
      sequence
      (map 'list #'identity sequence)))

(defmacro set-slot-if-unbound (object slot newval)
  "Sets the slot SLOT in OBJECT to NEWVAL if SLOT is unbound"
  (let ((obj-var (gensym))
        (slot-var (gensym)))
    `(let ((,obj-var ,object)
           (,slot-var ,slot))
       (unless (slot-boundp ,obj-var ,slot-var)
         (setf (slot-value ,obj-var ,slot-var) ,newval)))))

(?:defpattern stash (var pattern)
  "Saves form being matched by PATTERN into VAR."
  `(?:<> ,pattern ,var ,var))
