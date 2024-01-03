(cl:in-package #:cl-user)

(defun load-local-web-app ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/kindle/kindle-note-utility/web-app/")
  (push #p"/home/jfh/code/lisp/source/kindle/kindle-note-utility/web-app/" asdf:*central-registry*)
  (asdf:load-system "jfh-kindle-notes-web-app")
  ;; (in-package #:jfh-kindle-notes-web-app)
  )
