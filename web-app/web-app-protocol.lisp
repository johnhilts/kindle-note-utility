;;;; protocol related to specific web app concerns.
(cl:in-package #:jfh-kindle-notes-web-app)

(defclass web-app-user (jfh-app-core:application-user)
  ((%user-name :reader user-name
	       :initarg :user-name))
  (:documentation "Web Application user info."))
