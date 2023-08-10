;;;;
;;;; Regular expression tests
;;;;
(in-package #:com.kjcjohnson.synthkit/test)

(5am:def-suite :regular-expressions :in :smt-theories)
(5am:in-suite :regular-expressions)

(defun %in-re (str reglan)
  (smt:call-smt "str.in_re" str reglan))

(defun %not-in-re (str reglan)
  (not (smt:call-smt "str.in_re" str reglan)))

(defun %re-lit (str)
  (smt:call-smt "str.to_re" str))

(5am:test str.in_re/works
  (let ((reglan (%re-lit "asdf")))
    (5am:is (%in-re "asdf" reglan))
    (5am:is (%not-in-re " asdf" reglan))
    (5am:is (%not-in-re "asdf " reglan))))

(5am:def-test str.to_re/works ()
  (let ((reglan (smt:call-smt "str.to_re" "tEsT")))
    (5am:is (%in-re "tEsT" reglan)))) 

(5am:def-test re.none/works ()
  ;; None is the empty _set_, not the empty _string_
  (let ((reglan (smt:call-smt "re.none")))
    (5am:is (not (%in-re "" reglan)))
    (5am:is (not (%in-re "asdf" reglan)))
    (5am:is (not (%in-re "." reglan)))))

(5am:test re.all/works
  (let ((reglan (smt:call-smt "re.all")))
    (5am:is (%in-re "" reglan))
    (5am:is (%in-re (format nil "asdf~Cjkl;~Cq" #\Newline #\Newline) reglan))
    (5am:is (%in-re "asdf" reglan))
    (5am:is (%in-re "444...44.4.333" reglan))))

(5am:test re.allchar/works
  (let ((reglan (smt:call-smt "re.allchar")))
    (5am:is (%in-re "a" reglan))
    (5am:is (%in-re "4" reglan))
    (5am:is (%in-re " " reglan))
    (5am:is (%in-re (format nil "~C" #\Newline) reglan))
    (5am:is (%not-in-re "" reglan))
    (5am:is (%not-in-re "aa" reglan))))

(5am:test re.++/works
  (let ((reglan (smt:call-smt "re.++"
                              (%re-lit "asdf")
                              (smt:call-smt "re.allchar")
                              (%re-lit "jkl;"))))
    (5am:is (%not-in-re "asdf" reglan))
    (5am:is (%not-in-re "jkl;" reglan))
    (5am:is (%not-in-re "asdfjkl;" reglan))
    (5am:is (%not-in-re "asdfXjkl5" reglan))
    (5am:is (%in-re "asdfXjkl;" reglan))
    (5am:is (%in-re "asdf#jkl;" reglan))))
