;;;; start, stop web-app
(cl:in-package #:jfh-web-core)

(defmethod print-object ((web-configuration web-configuration) stream)
  "Print application configuration."
  (print-unreadable-object (web-configuration stream :type t)
    (with-accessors
          ((http-port http-port)
           (ssl-port ssl-port)
           (application-configuration jfh-app-core:application-configuration))
        web-configuration
      (format stream
              ;; "~a, ~a, ~a" http-port ssl-port application-configuration
	      ;; "~:[~:;Swank Port: ~:*~d~]~:[~:;, Swank Interface: ~:*~a~]~:*~:[~:;, ~]HTTP Port: ~d, ~:[~:;SSL Port: ~:*~d, ~]Settings File: ~s, User Path: ~s"
	      ;; swank-port swank-interface http-port ssl-port settings-file-path user-path-root
              "~A, HTTP Port: ~D~:[~:;, SSL Port: ~:*~D~]" application-configuration http-port ssl-port))))

(defmethod print-object ((web-application web-application) stream)
  "Print web application."
  (print-unreadable-object (web-application stream :type t)
    (with-accessors ((hunchentoot-acceptor hunchentoot-acceptor) (web-configuration web-configuration)) web-application
      (format stream
	      "Hunchentoot Acceptor: ~a, Configuration: ~a" hunchentoot-acceptor web-configuration))))

(defun make-web-configuration (&optional (ssl-port nil) (http-port 8080))
  "Get configuration info from the file system and hydrate web-configuration object.
Input: default configuration values.
Output: web-configuration object."
  (let* ((app-configuration (jfh-app-core:make-application-configuration))
         (call-back #'(lambda (settings)
			(if settings
			    (make-instance 'web-configuration
					   :ssl-port (getf settings :ssl-port)
					   :http-port (getf settings :http-port)
                                           :application-configuration app-configuration)
	                    (make-instance 'web-configuration
					   :ssl-port ssl-port
					   :http-port http-port
                                           :application-configuration app-configuration)))))
    (jfh-utility:fetch-or-create-data (jfh-app-core:settings-file-path app-configuration) call-back)))

(defmethod start-hunchentoot ((web-configuration web-configuration))
  "start or re-start the hunchentoot web server"
  (flet ((make-acceptor-instance ()
           (with-accessors ((ssl-port ssl-port) (http-port http-port)) web-configuration
             (if ssl-port
                 (make-instance 'tbnl:easy-ssl-acceptor :port ssl-port :ssl-privatekey-file #P"../certs/server.key" :ssl-certificate-file #P"../certs/server.crt")
                 (make-instance 'tbnl:easy-acceptor :port http-port)))))
    (prog1
	(tbnl:start (make-acceptor-instance))
      (format t "~&huncentoot started~%" ))))

(defmethod make-web-application ((hunchentoot-acceptor tbnl:easy-acceptor) (web-configuration web-configuration))
  "Constructor for web-application"
  (make-instance 'web-application
		 :hunchentoot-acceptor hunchentoot-acceptor
		 :web-configuration web-configuration))

(defun add-static-content-handlers ()
  "Add handlers for provided static content web/path mappings."
  (mapc
   (lambda (mapping)
     (push
      (tbnl:create-static-file-dispatcher-and-handler (car mapping) (cdr mapping))
      tbnl:*dispatch-table*))
   *static-path-maps*)
  ;; (pushnew (tbnl:create-static-file-dispatcher-and-handler
  ;;        "/favicon.ico" "ez-favicon.ico") *dispatch-table*)
  ;; (push (tbnl:create-static-file-dispatcher-and-handler
  ;;        "/styles.css" "static/styles.css") *dispatch-table*)
  )

(defparameter *static-path-maps* ())
(defun add-static-path-map (web-path physical-path)
  "add mapping pairs to be used to expose static assets from the web server."
  (pushnew
   (cons web-path physical-path)
   *static-path-maps*
   :key #'car
   :test #'string=))

;; TODO - add restart if http or ssl port is in use
(defmethod start-web-app ((web-configuration web-configuration))
  "Input: application-configuration object and path maps for static assets. Output: web-application object. This will start the web application running on top of hunchentoot."
  (setf tbnl:*session-max-time* (* 24 7 60 60))
  (setf tbnl:*rewrite-for-session-urls* nil)
  (add-static-content-handlers)
  (make-web-application (start-hunchentoot web-configuration) web-configuration))
;; how to find: (find-method #'start-web-app nil (list (find-class 'application-configuration)))

;; ;; TODO - add restart if swank port is in use
;; (defmethod start-web-app ((web-configuration web-configuration))
;;   "Input: application-configuration object. Output: web-application object. This will start the web application running on top of hunchentoot, and optionally start swank."
;;   (when (jfh-app-core:swank-port web-configuration)
;;     (jfh-app-core:start-swank web-configuration))
;;   (setf tbnl:*session-max-time* (* 24 7 60 60))
;;   (setf tbnl:*rewrite-for-session-urls* nil)
;;   ;; (publish-static-content)
;;   ;; (let ((user-index-path (format nil "~a/user-index.sexp" *users-root-folder-path*)))
;;   ;;   (ensure-directories-exist user-index-path))
;;   ;; (format t "~&loaded user info~%" )
;;   (make-web-application (start-hunchentoot web-configuration) web-configuration))
;; ;; how to find: (find-method #'start-web-app nil (list (find-class 'application-configuration)))

(defmethod stop-hunchentoot ((web-application web-application))
  "Input: web-application. Stop hunchentoot web-server via the provided web-application object."
  (tbnl:stop (hunchentoot-acceptor web-application)))

;; (defmethod stop-web-app ((web-application web-application) &optional (stop-swank nil))
;;   "Input: web-application and application-configuration objects. Output: #:web-app-stopped. This will stop the web application, and optionally stop swank. The HTTP port will be released"
;;   (stop-hunchentoot web-application)
;;   (when (and
;; 	 stop-swank
;; 	 (jfh-app-core:swank-port (web-configuration web-application)))
;;     (jfh-app-core:stop-swank (web-configuration web-application)))
;;   '#:web-app-stopped)

(defvar *web-application*)

(defun web-application-shell ()
  "Use this to start the web application."
  (let* ((web-configuration (make-web-configuration)))
    (setf *web-application* (start-web-app web-configuration))))

(defun %web-app-running-p ()
  "Check if the web app - actually huncentoot server - is running.
This is meant to be a convenience function;
NOTE use of non-exported symbol."
  (null (tbnl::acceptor-shutdown-p (hunchentoot-acceptor *web-application*))))

(defun %clear-static-routes ()
  "Just a convenience method!"
  (setq tbnl:*dispatch-table* (last tbnl:*dispatch-table*)))
