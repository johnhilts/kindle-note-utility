(cl:in-package #:asdf-user)

(defsystem #:jfh-kindle-notes-main
  :description "Utility to do things with notes made on a kindle e-reader."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:jfh-kindle-notes #:jfh-kindle-notes-web-app #:jfh-web-core #:jfh-app-core #:jfh-utility)
  :components ((:file package)
               (:file main)))

