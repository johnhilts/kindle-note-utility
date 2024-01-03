(in-package #:cl-user)

(defpackage #:jfh-app-core
  (:use #:common-lisp)
  (:export
   #:application-configuration
   #:make-application-configuration
   #:start-swank
   #:stop-swank
   #:swank-port
   #:swank-interface
   #:settings-file-path
   #:user-path-root))
