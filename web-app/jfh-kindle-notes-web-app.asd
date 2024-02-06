(cl:in-package #:asdf-user)

(defsystem #:jfh-kindle-notes-web-app
  :description "Utility to do things with notes made on a kindle e-reader."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:parenscript #:cl-json #:cl-who #:jfh-utility #:jfh-web-core)
  :components ((:file package)
               (:file configure)
               (:file web-app-protocol)
               (:file user)
	       (:file auth)
               (:file upload)
               (:file pages)
               (:file kindle-entry-api)))

