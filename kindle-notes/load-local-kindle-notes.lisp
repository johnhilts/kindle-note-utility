(cl:in-package #:cl-user)

(defun load-local-kindle-notes ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/kindle/kindle-note-utility/kindle-notes/")
  (push #p"/home/jfh/code/lisp/source/kindle/kindle-note-utility/kindle-notes/" asdf:*central-registry*)
  (asdf:load-system "jfh-kindle-notes")
  ;; (in-package #:jfh-kindle-notes)
  )
