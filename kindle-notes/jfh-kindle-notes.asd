(cl:in-package #:asdf-user)

(defsystem #:jfh-kindle-notes
  :description "Utility to do things with notes made on a kindle e-reader."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file package)
               (:file kindle-note-protocol)
               (:file kindle-notes-util)
               (:file kindle-notes)))

