;;;;
;;;; Theory definitions for strings
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defun str-++ (str1 str2)
  "String contatenation"
  (declare (type string str1 str2))
  (concatenate 'string str1 str2))

(defun str-replace (str old new)
  "Replaces OLD with NEW in STR"
  (declare (simple-string str old new)
           (optimize (speed 3)))
  (let ((ix (search old str)))
    (if (null ix)
        str
        (concatenate 'string
                     (subseq str 0 ix)
                     new
                     (subseq str (+ ix (length old)))))))

(defun str-at (str ix)
  "Gets the character at IX from STR"
  (declare (type string str)
           (type integer ix))
  (if (and (< ix (length str)) (>= ix 0))
      (string (char str ix))
      ""))

(defun str-from-int (int)
  "Gets a string with the text of INT"
  (declare (type integer int))
  (princ-to-string int))

(defun str-substr (str start end)
  "Gets a substring of STR from START to END"
  (declare (type string str)
           (type integer start end))
  (str:substring start end str))

(defun str-len (str)
  "Gets the length of STR"
  (declare (type string str))
  (length str))

(defun str-to-int (str)
  "Gets the integer value of STR"
  (declare (type string str))
  (or (parse-integer str :junk-allowed t) -1))

(defun str-indexof (str what start)
  "Gets the index of WHAT in STR starting from position START"
  (declare (type string str what)
           (type integer start))
  (if (or (minusp start) (> start (length str)))
      -1
      (let ((val (search what str :start2 start)))
        (if (null val)
            -1
            val))))

(defun str-prefixof (prefix str)
  "Checks if STR starts with PREFIX"
  (declare (type string str prefix))
  (str:starts-with? prefix str))

(defun str-suffixof (suffix str)
  "Checks if STR ends with SUFFIX"
  (declare (type string str suffix))
  (str:ends-with? suffix str))

(defun str-contains (str what)
  "Checks if STR contains WHAT"
  (declare (type string str what))
  (str:contains? what str))
