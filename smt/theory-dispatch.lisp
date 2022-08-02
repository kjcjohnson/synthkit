;;;;
;;;; Theory dispatch - looks up function for symbol
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(let ((data
        `(
          ;; Core functions
          ("true"     . ,#'core-true)
          ("false"    . ,#'core-false)
          ("not"      . ,#'core-not)
          ("or"       . ,#'core-or)
          ("and"      . ,#'core-and)
          ("=>"       . ,#'core-=>)
          ("xor"      . ,#'core-xor)
          ("="        . ,#'core-=)
          ("distinct" . ,#'core-distinct)
          ("ite"      . ,#'core-ite)
          
          ;; Integer functions
          ("+" . ,#'ints-+)
          ("-" . ,#'ints--)
          ("<" . ,#'ints-<)
          
          ;; String functions
          ("str.++"       . ,#'str-++)
          ("str.replace"  . ,#'str-replace)
          ("str.at"       . ,#'str-at)
          ("str.from_int" . ,#'str-from-int)
          ("str.substr"   . ,#'str-substr)
          ("str.len"      . ,#'str-len)
          ("str.to_int"   . ,#'str-to-int)
          ("str.indexof"  . ,#'str-indexof)
          ("str.prefixof" . ,#'str-prefixof)
          ("str.suffixof" . ,#'str-suffixof)
          ("str.contains" . ,#'str-contains)
          
          ;; Bit vector functions
          ("bvnot"  . ,#'bv-not)
          ("bvand"  . ,#'bv-and)
          ("bvor"   . ,#'bv-or)
          ("bvxor"  . ,#'bv-xor)
          ("bvlshr" . ,#'bv-lshr)
          ("bvshl"  . ,#'bv-shl)
          ("bvadd"  . ,#'bv-add)
          )))

  (defun lookup-theory-function (name)
    "Looks up an SMT theory function"
    (flet ((ensure-list (thing)
             "Ensures that THING is a list. Makes a single element list if not."
             (if (consp thing)
                 thing
                 (list thing))))
      
      (setf name (ensure-list name))
      
      (let ((looked-up (assoc name data
                              :key #'ensure-list
                              :test #'equal)))
        
        (unless (null looked-up)
          (return-from lookup-theory-function (cdr looked-up)))
        
        ;; Not a theory function
        nil)))

  (defun map-built-in-definitions (operation)
    "Iterates over built-in function definitions. OPERATION accepts two args"
    (dolist (defn-form data)
      (funcall operation (car defn-form) (cdr defn-form)))))
