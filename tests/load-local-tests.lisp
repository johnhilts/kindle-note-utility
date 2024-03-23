(cl:in-package #:cl-user)

(defun load-local-tests ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/kindle/kindle-note-utility/tests/")
  (push #p"/home/jfh/code/lisp/source/kindle/kindle-note-utility/tests/" asdf:*central-registry*)
  (asdf:load-system "jfh-kindle-notes-tests")
  ;; (in-package #:jfh-kindle-notes-tests)
  )
