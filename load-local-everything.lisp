(cl:in-package #:cl-user)

(defun load-local-everything ()
  (load "/home/jfh/code/lisp/source/kindle/kindle-note-utility/utility/load-local-utility.lisp")
  (load-local-utility)
  (print "utilty loaded")

  (load "/home/jfh/code/lisp/source/kindle/kindle-note-utility/app-core/load-local-app-core.lisp")
  (load-local-app-core)
  (print "app-core loaded")

  (load "/home/jfh/code/lisp/source/kindle/kindle-note-utility/kindle-notes/load-local-kindle-notes.lisp")
  (load-local-kindle-notes)
  (print "kindle-notes loaded")

  (load "/home/jfh/code/lisp/source/kindle/kindle-note-utility/web-core/load-local-web-core.lisp")
  (load-local-web-core)
  (print "web-core loaded")

  (load "/home/jfh/code/lisp/source/kindle/kindle-note-utility/web-auth/load-local-web-auth.lisp")
  (load-local-web-auth)
  (print "web-auth loaded")

  (load "/home/jfh/code/lisp/source/kindle/kindle-note-utility/web-app/load-local-web-app.lisp")
  (load-local-web-app)
  (print "web-app loaded")

  (swank:set-default-directory "/home/jfh/code/lisp/source/kindle/kindle-note-utility/")
  (push #p"/home/jfh/code/lisp/source/kindle/kindle-note-utility/" asdf:*central-registry*)
  (asdf:load-system "jfh-kindle-notes-main")
  (print "main loaded")
  ;; (in-package #:jfh-kindle-notes-main)
  )
