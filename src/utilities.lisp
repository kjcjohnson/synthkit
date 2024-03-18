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

(?:defpattern stash (var pattern)
  "Saves form being matched by PATTERN into VAR."
  `(?:<> ,pattern ,var ,var))

(defmacro with-timing ((var) &body body)
  "Reports timing data"
  (let ((start-time-var (gensym)))
    `(let ((,start-time-var (get-internal-real-time)))
       (symbol-macrolet ((,var (/ (- (get-internal-real-time) ,start-time-var)
                                  internal-time-units-per-second)))
         ,@body))))

(defstruct section-timing-data
  "Contains information about performance data for a section"
  (real-time 0 :type integer)
  (gc-time 0 :type integer)
  (cons-count 0 :type integer))

(defmacro declare-timed-section (name &optional (doc ""))
  "Declares the name of a timed section"
  (declare (type symbol name))
  `(progn
     #-sbcl
     (defvar ,name (make-section-timing-data) ,doc)
     #+sbcl
     (sb-ext:defglobal ,name (make-section-timing-data) ,doc)
     (declaim (type section-timing-data ,name))))

#+sbcl
(defmacro with-timed-section ((timing-place) &body body)
  "Times BODY, accumulating into TIMING-PLACE"
  (let ((real-time-start-var (gensym))
        (gc-time-start-var (gensym))
        (cons-count-start-var (gensym)))
    `(let ((,real-time-start-var (get-internal-real-time))
           (,gc-time-start-var sb-ext:*gc-run-time*)
           (,cons-count-start-var (sb-ext:get-bytes-consed)))
       (declare (integer ,real-time-start-var
                         ,gc-time-start-var
                         ,cons-count-start-var))
       (unwind-protect
            (progn ,@body)
         (incf (section-timing-data-real-time ,timing-place)
               (- (get-internal-real-time) ,real-time-start-var))
         (incf (section-timing-data-gc-time ,timing-place)
               (- sb-ext:*gc-run-time* ,gc-time-start-var))
         (incf (section-timing-data-cons-count ,timing-place)
               (- (sb-ext:get-bytes-consed) ,cons-count-start-var))))))

#-sbcl
(defmacro with-timed-section ((timing-place) &body body)
  (declare (ignore timing-place))
  `(progn ,@body))

(defun get-timed-section-real-time (timing-place &key (internal nil))
  "Gets the timing data from TIMING-PLACE. If INTERNAL, return in internal time units"
  (let ((delta-time (section-timing-data-real-time timing-place)))
    (if internal
        delta-time
        (/ delta-time internal-time-units-per-second))))

(defun get-timed-section-gc-time (timing-place &key (internal nil))
  "Gets the GC timing data from TIMING-PLACE"
  (let ((delta-time (section-timing-data-gc-time timing-place)))
    (if internal
        delta-time
        (/ delta-time internal-time-units-per-second))))

(defun get-timed-section-bytes-consed (timing-place &key unit)
  "Gets the number of bytes consed"
  (check-type unit (or null string))
  (if (null unit)
      (section-timing-data-cons-count timing-place)
      (let ((divisor (str:string-case (str:downcase unit)
                       ("kib" 1024)
                       ("mib" (* 1024 1024))
                       ("gib" (* 1024 1024 1024))
                       (otherwise (error "Unknown unit: ~a" unit)))))
        (/ (section-timing-data-cons-count timing-place) divisor))))

(defun reset-timed-section-time (timing-place)
  "Resets the timing data from TIMING-PLACE"
  (setf timing-place 0))
