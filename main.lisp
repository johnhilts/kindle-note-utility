(in-package #:jfh-kindle-notes-main)

(defun application-shell ()
  "Use this to start the application."
  (mapc
   (lambda (pair)
     (jfh-web-core:add-static-path-map (car pair) (cadr pair)))
   jfh-kindle-notes-web-app:*static-paths-maps*)

  (let ((web-application (jfh-web-core:web-application-shell)))

    (jfh-app-core:start-swank (jfh-app-core:application-configuration (jfh-web-core:web-configuration web-application)))))



