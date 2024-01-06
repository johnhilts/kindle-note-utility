(in-package #:cl-user)

(defpackage #:jfh-web-auth
  (:use #:common-lisp)
  (:local-nicknames (#:web #:jfh-web-core))
  (:export
   #:define-protected-page
   #:authenticated-user
   #:web-auth-pages
   #:*web-auth-pages*
   #:show-auth-failure
   #:find-user-info
   #:login-page
   #:logout-page
   #:signup-page))
