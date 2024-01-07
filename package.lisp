(in-package #:cl-user)

(defpackage #:jfh-kindle-notes-main
  (:use #:common-lisp)
  (:local-nicknames (#:web #:jfh-web-core) (#:auth #:jfh-web-auth) (#:web-app #:jfh-kindle-notes-web-app))
  (:export
   #:application-shell))
