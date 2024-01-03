(cl:in-package #:cl-user)

(defun load-local-main ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/kindle/kindle-note-utility/")
  (push #p"/home/jfh/code/lisp/source/kindle/kindle-note-utility/" asdf:*central-registry*)
  (asdf:load-system "jfh-kindle-notes-main")
  ;; (in-package #:jfh-kindle-notes-main)
  )
