;;;; protocol related to web application
(cl:in-package #:jfh-kindle-notes-web-app)

(defclass application-configuration ()
  ((%swank-port :reader swank-port
		:initarg :swank-port)
   (%swank-interface :reader swank-interface
		     :initarg :swank-interface)
   (%http-port :reader http-port
	       :initarg :http-port)
   (%ssl-port :reader ssl-port
	      :initarg :ssl-port)
   (%settings-file-path :reader settings-file-path
			:initarg :settings-file-path))
  (:documentation "Application configurations."))

(defclass web-application ()
  ((%hunchentoot-acceptor :reader hunchentoot-acceptor
			  :initarg :hunchentoot-acceptor)
   (%application-configuration :reader application-configuration
			       :initarg :application-configuration))
  (:documentation "Web application."))

(defgeneric start-swank (application-configuration)
  (:documentation "Input: application-configuration. Start swank with the provided configuration settings."))

(defgeneric start-hunchentoot (application-configuration)
  (:documentation "Input: application-configuration. Start hunchentoot web-server with the provided configuration settings."))

(defgeneric start-web-app (application-configuration)
  (:documentation "Input: application-configuration object. Output: web-application object. This will start the web application running on top of hunchentoot, and optionally start swank."))
;; (documentation 'start-web-app 'function)

(defgeneric stop-hunchentoot (web-application)
  (:documentation "Input: web-application. Stop hunchentoot web-server via the provided web-application object."))

(defgeneric stop-swank (application-configuration)
  (:documentation "Input: application-configuration. Stop swank with the provided configuration settings."))

(defgeneric stop-web-app (web-application &optional stop-swank)
  (:documentation "Input: web-application objects. Output: #:web-app-stopped. This will stop the web application, and optionally stop swank. The HTTP port will be released. Also, optionally stop the swank server."))
;; (documentation 'stop-web-app 'function)

(defgeneric make-application-configuration (application-configuration)
  (:documentation "Input: application-configuration (default settings) object. Output application-configuration object."))

(defgeneric make-web-application (tbnl:easy-acceptor application-configuration)
  (:documentation "Input: hunchentoot easy-acceptor, application-configuration (default settings) object. Output web-application object."))

