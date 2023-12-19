;;;; start, stop web-app
(cl:in-package #:jfh-kindle-notes-web-app)

(defmethod print-object ((application-configuration application-configuration) stream)
  "Print application configuration."
  (print-unreadable-object (application-configuration stream :type t)
    (with-accessors ((swank-port swank-port) (swank-interface swank-interface) (http-port http-port) (ssl-port ssl-port) (settings-file-path settings-file-path)) application-configuration
      (format stream
	      "~:[~:;Swank Port: ~:*~d~]~:[~:;, Swank Interface: ~:*~a~]~2:*~:[~:;, ~]HTTP Port: ~d, ~:[~:;SSL Port: ~:*~d, ~]Settings File: ~s"
	      swank-port swank-interface http-port ssl-port settings-file-path))))

(defmethod print-object ((web-application web-application) stream)
  "Print web application."
  (print-unreadable-object (web-application stream :type t)
    (with-accessors ((hunchentoot-acceptor hunchentoot-acceptor) (application-configuration application-configuration)) web-application
      (format stream
	      "Hunchentoot Acceptor: ~a, Configuration: ~a" hunchentoot-acceptor application-configuration))))

(defun make-default-settings ()
  "Constructor for application configuration object with default settings."
  (make-instance 'application-configuration
		 :swank-port 4005
		 :swank-interface "localhost"
		 :ssl-port nil
		 :http-port 8080
		 :settings-file-path "./settings.sexp"))

(defmethod make-application-configuration ((default-settings application-configuration))
  "Get configuration info from the file system and hydrate application-configuration object.
Input: default-settings object.
Output: application-configuration object."
  (let* ((call-back #'(lambda (settings)
			(if settings
			    (make-instance 'application-configuration
					   :swank-port (getf settings :swank-port)
					   :swank-interface (getf settings :swank-interface)
					   :ssl-port (getf settings :ssl-port)
					   :http-port (getf settings :http-port)
					   :settings-file-path (settings-file-path default-settings))
			    default-settings))))
    (jfh-kindle-notes-server-infrastructure:fetch-or-create-data (settings-file-path default-settings) call-back)))

(defmethod start-swank ((application-configuration application-configuration))
  "Input: application-configuration. Start swank on the configured port."
  (with-accessors ((swank-port swank-port) (swank-interface swank-interface)) application-configuration
    (let ((*debug-io* (make-broadcast-stream)))
      (swank:create-server :port swank-port
			   :interface swank-interface
                           :dont-close t)
      (format t "Started swank at port: ~A." swank-port))))

(defmethod start-hunchentoot ((application-configuration application-configuration))
  "start or re-start the hunchentoot web server"
  (flet ((make-acceptor-instance ()
           (with-accessors ((ssl-port ssl-port) (http-port http-port)) application-configuration
             (if ssl-port
                 (make-instance 'tbnl:easy-ssl-acceptor :port ssl-port :ssl-privatekey-file #P"../certs/server.key" :ssl-certificate-file #P"../certs/server.crt")
                 (make-instance 'tbnl:easy-acceptor :port http-port)))))
    (prog1
	(tbnl:start (make-acceptor-instance))
      (format t "~&huncentoot started~%" ))))

(defmethod make-web-application ((hunchentoot-acceptor tbnl:easy-acceptor) (application-configuration application-configuration))
  "Constructor for web-application"
  (make-instance 'web-application
		 :hunchentoot-acceptor hunchentoot-acceptor
		 :application-configuration application-configuration))

;; TODO - add restart if swank port is in use
(defmethod start-web-app ((application-configuration application-configuration))
  "Input: application-configuration object. Output: web-application object. This will start the web application running on top of hunchentoot, and optionally start swank."
  (when (swank-port application-configuration)
    (start-swank application-configuration))
  (setf tbnl:*session-max-time* (* 24 7 60 60))
  (setf tbnl:*rewrite-for-session-urls* nil)
  ;; (publish-static-content)
  ;; (let ((user-index-path (format nil "~a/user-index.sexp" *users-root-folder-path*)))
  ;;   (ensure-directories-exist user-index-path))
  ;; (format t "~&loaded user info~%" )
  (make-web-application (start-hunchentoot application-configuration) application-configuration))
;; how to find: (find-method #'start-web-app nil (list (find-class 'application-configuration)))

(defmethod stop-hunchentoot ((web-application web-application))
  "Input: web-application. Stop hunchentoot web-server via the provided web-application object."
  (tbnl:stop (hunchentoot-acceptor web-application)))

(defmethod stop-swank ((application-configuration application-configuration))
  (with-accessors ((swank-port swank-port)) application-configuration
    (when swank-port
      (swank:stop-server swank-port)
      (format t "Stopped swank at port: ~A." swank-port))))

(defmethod stop-web-app ((web-application web-application) &optional (stop-swank nil))
  "Input: web-application and application-configuration objects. Output: #:web-app-stopped. This will stop the web application, and optionally stop swank. The HTTP port will be released"
  (stop-hunchentoot web-application)
  (when (and
	 stop-swank
	 (swank-port (application-configuration web-application)))
    (stop-swank (application-configuration web-application)))
  '#:web-app-stopped)

(defvar *web-application*)

(defun web-application-shell ()
  "Use this to start the web application."
  (let* ((default-settings (make-default-settings))
	 (application-configuration (make-application-configuration default-settings)))
    (setf *web-application* (start-web-app application-configuration))))
