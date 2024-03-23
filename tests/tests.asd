(cl:in-package #:asdf-user)

(defsystem #:jfh-kindle-notes-tests
  :description "Tests: thread safety stress test."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:bordeaux-threads)
  :components ((:file package)
               (:file threads)))

