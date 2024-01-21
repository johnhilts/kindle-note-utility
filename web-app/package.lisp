(in-package #:cl-user)

(defpackage #:jfh-kindle-notes-web-app
  (:use #:common-lisp)
  (:local-nicknames (#:web #:jfh-web-core) (#:auth #:jfh-web-auth))
  (:export
   #:*web-configuration*
   #:*static-paths-maps*
   #:signup-page
   #:login-page
   #:find-user-info
   #:show-auth-failure))
