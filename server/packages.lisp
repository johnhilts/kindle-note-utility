(in-package #:cl-user)

(defpackage #:jfh-kindle-notes-web-app
  (:use #:common-lisp)
  (:export
   #:web-application-shell))

(defpackage #:jfh-kindle-notes-server-infrastructure
  (:use #:common-lisp)
  (:export
   #:fetch-or-create-data
   #:serialize-to-json))
