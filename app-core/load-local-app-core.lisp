(cl:in-package #:cl-user)

(defun load-local-app-core ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/kindle/kindle-note-utility/app-core/")
  (push #p"/home/jfh/code/lisp/source/kindle/kindle-note-utility/app-core/" asdf:*central-registry*)
  (asdf:load-system "jfh-app-core")
  ;; (in-package #:jfh-app-core)
  )
