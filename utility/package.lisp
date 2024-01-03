(in-package #:cl-user)

(defpackage #:jfh-utility
  (:use #:common-lisp)
  (:export
   #:fetch-or-create-data
   #:write-complete-file
   #:generate-unique-token
   #:hash-password
   #:serialize-to-json))
