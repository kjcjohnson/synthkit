;;;;
;;;; Theory definitions for strings
;;;;
(in-package #:com.kjcjohnson.synthkit.smt)

(defsmtfun "str.++" :strings (str1 str2)
  "String contatenation"
  (declare (type string str1 str2))
  (concatenate 'string str1 str2))

(defsmtfun "str.replace" :strings (str old new)
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

(defsmtfun "str.at" :strings (str ix)
  "Gets the character at IX from STR"
  (declare (type string str)
           (type integer ix))
  (if (and (< ix (length str)) (>= ix 0))
      (string (char str ix))
      ""))

(defsmtfun "str.from_int" :strings (int)
  "Gets a string with the text of INT"
  (declare (type integer int))
  (princ-to-string int))

(defsmtfun "str.substr" :strings (str start end)
  "Gets a substring of STR from START to END"
  (declare (type string str)
           (type integer start end))
  (str:substring start end str))

(defsmtfun "str.len" :strings (str)
  "Gets the length of STR"
  (declare (type string str))
  (length str))

(defsmtfun "str.to_int" :strings (str)
  "Gets the integer value of STR"
  (declare (type string str))
  (or (parse-integer str :junk-allowed t) -1))

(defsmtfun "str.indexof" :strings (str what start)
  "Gets the index of WHAT in STR starting from position START"
  (declare (type string str what)
           (type integer start))
  (if (or (minusp start) (> start (length str)))
      -1
      (let ((val (search what str :start2 start)))
        (if (null val)
            -1
            val))))

(defsmtfun "str.prefixof" :strings (prefix str)
  "Checks if STR starts with PREFIX"
  (declare (type string str prefix))
  (str:starts-with? prefix str))

(defsmtfun "str.suffixof" :strings (suffix str)
  "Checks if STR ends with SUFFIX"
  (declare (type string str suffix))
  (str:ends-with? suffix str))

(defsmtfun "str.contains" :strings (str what)
  "Checks if STR contains WHAT"
  (declare (type string str what))
  (str:contains? what str))

;;;
;;; Basic regex functions
;;;
;;; We use the cl-ppcre parse trees to represent regular languages, in a wrapper type
;;;
(defstruct (regular-language (:constructor %reglan (parse-tree)))
  parse-tree)

(defun %parse-tree (reglan)
  (check-type reglan regular-language)
  (regular-language-parse-tree reglan))

;;;
;;; Patterns for matching regular expression constructs
;;;
(?:defpattern re.* (child)
  "Matches a Kleene closure in a regex parse tree"
  `(list :greedy-repetition 0 nil ,child))

(?:defpattern re.opt (child)
  "Matches an optional element in a regex parse tree"
  `(list :greedy-repetition 0 1 ,child))

(?:defpattern re.union (left right &rest rest)
  "Matches a two-element union"
  `(list :alternation ,left ,right ,@rest))

;;;
;;; Definitions
;;;
(defsmtfun "str.in_re" :strings (str reglan)
  "Checks if STR is in the regular language REGLAN"
  (check-type str string)
  (check-type reglan regular-language)
  (let ((parse-tree (%parse-tree reglan)))
    ;;(format t "REGEX: ~s on [~a]~%" parse-tree str)
    (if (null (ppcre:scan `(:group
                            (:flags :single-line-mode-p)
                            :start-anchor
                            ,parse-tree
                            :end-anchor)
                          str))
        nil ; Normalize boolean values
        t)))

(defsmtfun "str.to_re" :strings (str)
  "Converts STR to a regex literal"
  (check-type str string)
  ;; Regex literals are just strings
  (%reglan str))

(defsmtfun "re.none" :strings ()
  "The empty expression"
  (%reglan :void))

(defsmtfun "re.all" :strings ()
  "The set of all strings"
  (%reglan '(:greedy-repetition 0 nil :everything)))

(defsmtfun "re.allchar" :strings ()
  "The set of all single-character strings"
  (%reglan :everything))

(defsmtfun "re.++" :strings (reglan1 reglan2 &rest reglans)
  "Concatentates regular languages in sequence"
  (%reglan
   `(:sequence
     ,(%parse-tree reglan1)
     ,(%parse-tree reglan2)
     ,@(map 'list #'%parse-tree reglans))))

(defsmtfun "re.union" :strings (reglan1 reglan2 &rest reglans)
  "Union of regular languages"
  (%reglan
   `(:alternation
     ,(%parse-tree reglan1)
     ,(%parse-tree reglan2)
     ,@(map 'list #'%parse-tree reglans))))

(defsmtfun "re.intersection" :strings (reglan1 reglan2 &rest reglans)
  "Intersection of regular languages"
  (declare (ignore reglan1 reglan2 reglans))
  (error "Huh?"))

(defsmtfun "re.*" :strings (reglan)
  "Kleene closure"
  ;; Apparent bug in CL-PPCRE causes stack overflow on some nested repetitions
  ;; so if we get a nested redundant repetitions, combine with this new one.
  (let ((child-tree (%parse-tree reglan)))
    (?:match child-tree
      ;; ((<x>)*)* --> (<x>)*
      ((re.* _) reglan)
      ;; ((<x>)?)* --> (<x>)*
      ((re.opt subtree) (%reglan `(:greedy-repetition 0 nil ,subtree)))
      ;; ((<x>)*|(<y>)*)* --> (<x>|<y>)*
      ((re.union (or (re.* x) (re.opt x)) (or (re.* y) (re.opt y)))
       (%reglan `(:greedy-repetition 0 nil (:alternation ,x ,y))))
      ;; (<x>|(<y>)*)* --> (<x>|<y>)* [and variants]
      ((or (re.union x (or (re.* y) (re.opt y)))
           (re.union (or (re.* y) (re.opt y)) x))
       (%reglan `(:greedy-repetition 0 nil (:alternation ,x ,y))))
      ;; Anything else is fine for now
      (otherwise
       (%reglan `(:greedy-repetition 0 nil ,(%parse-tree reglan)))))))

;; re.diff

(defsmtfun "re.+" :strings (reglan)
  "Kleene plus"
  (%reglan
   `(:greedy-repetition 1 nil ,(%parse-tree reglan))))

(defsmtfun "re.opt" :strings (reglan)
  "Optional element"
  (let ((child-tree (%parse-tree reglan)))
    (?:match child-tree
      ;; ((<x>)?)? --> (<x>)?
      ((re.opt _) reglan)
      ;; ((<x>)*)? --> (<x>)*
      ((re.* _) reglan)
      ;; Anything else is fine for now
      (otherwise
       (%reglan `(:greedy-repetition 0 1 ,(%parse-tree reglan)))))))

;; Empty character classes aren't allowed, so define a filter to do it
(defun %empty-char-class-filter (pos)
  "Always fail."
  (declare (ignore pos))
  nil)

(defsmtfun "re.range" :strings (strl stru)
  "Character class from strl to stru"
  (check-type strl string)
  (check-type stru string)
  (if (and (= 1 (length strl))
           (= 1 (length stru)))
      `(%reglan (:char-class (:range ,(elt strl 0) ,(elt stru 0))))
      `(%reglan (:filter ,#'%empty-char-class-filter))))

;; (_ re.^ n)

;; (_ re.loop n1 n2)
