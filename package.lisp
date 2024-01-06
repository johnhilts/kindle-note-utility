(in-package #:cl-user)

(defpackage #:jfh-kindle-notes-main
  (:use #:common-lisp)
  (:local-nicknames (#:web #:jfh-web-core) (#:auth #:jfh-web-auth))
  (:export
   #:application-shell))
