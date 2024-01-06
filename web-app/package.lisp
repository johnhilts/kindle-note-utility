(in-package #:cl-user)

(defpackage #:jfh-kindle-notes-web-app
  (:use #:common-lisp)
  (:local-nicknames (#:web #:jfh-web-core) (#:auth #:jfh-web-auth))
  (:export
   #:*static-paths-maps*))
