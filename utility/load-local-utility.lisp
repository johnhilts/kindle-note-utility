(cl:in-package #:cl-user)

(defun load-local-utility ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/kindle/kindle-note-utility/utility/")
  (push #p"/home/jfh/code/lisp/source/kindle/kindle-note-utility/utility/" asdf:*central-registry*)
  (asdf:load-system "jfh-utility")
  ;; (in-package #:jfh-utility)
  )
