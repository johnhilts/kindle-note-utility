(in-package #:jfh-kindle-notes-main)

(defvar *web-configuration*)

(defun register-web-auth-functions ()
  "Register functions using provided class to enable web-auth features."
  (make-instance 'auth:web-auth-pages
		 :signup-page #'web-app:signup-page
		 :login-page #'web-app:login-page
		 :find-user-info #'web-app:find-user-info
		 :show-auth-failure #'web-app:show-auth-failure))

(defun application-shell ()
  "Use this to start the application."
  (mapc
   (lambda (pair)
     (jfh-web-core:add-static-path-map (car pair) (cadr pair)))
   jfh-kindle-notes-web-app:*static-paths-maps*)

  (let ((web-application (jfh-web-core:web-application-shell)))

    (setf *web-configuration* (web:web-configuration web-application))
    (auth:use-web-auth (auth:web-auth-pages))
    
    (jfh-app-core:start-swank (jfh-app-core:application-configuration (jfh-web-core:web-configuration web-application)))

    web-application))

;; TODO - add call to instantiate web-auth instance
;; (auth:use-web-auth (auth:web-auth-pages))
;; (funcall (function auth:*web-auth-pages*))
