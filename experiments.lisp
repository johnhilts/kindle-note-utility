(in-package #:cl-user)

(defpackage #:jfh/experiments
  (:use #:common-lisp))

(in-package #:jfh/experiments)

;; a limited way to do string replacement with standard code
(defun string-replace (&optional (string-pairs '(("big" . "small"))) (string '("big" "red" "one")))
  (sublis string-pairs
          string
          :test (lambda (a b) (if (and (stringp a) (stringp b)) (string-equal a b) nil))))
