(in-package #:cl-user)

(defpackage #:jfh-web-core
  (:use #:common-lisp)
  (:export
   #:web-application-shell
   #:web-configuration
   #:static-root
   #:add-static-path-map
   #:define-api-endpoint
   #:verb))
