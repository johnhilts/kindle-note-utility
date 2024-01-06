(cl:in-package #:cl-user)

(defun load-local-web-core ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/kindle/kindle-note-utility/web-core/")
  (push #p"/home/jfh/code/lisp/source/kindle/kindle-note-utility/web-core/" asdf:*central-registry*)
  (asdf:load-system "jfh-web-core")
  ;; (in-package #:jfh-web-core)
  )
