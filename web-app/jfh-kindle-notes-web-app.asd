(cl:in-package #:asdf-user)

(defsystem #:jfh-kindle-notes-web-app
  :description "Utility to do things with notes made on a kindle e-reader."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:parenscript #:cl-json #:cl-who #:jfh-utility #:jfh-web-core)
  :components ((:file package)
               (:file common/configure)
               (:file common/web-app-protocol)
	       (:file notes/utility)
               (:file common/user)
               (:file notes/page-include)
	       (:file common/auth)
	       (:file notes/notes)
               (:file notes/upload)
               (:file notes/page-handlers)
               (:file notes/api-handlers)))

