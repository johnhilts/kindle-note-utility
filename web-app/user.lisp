;;;; functions to support web app users.
(cl:in-package #:jfh-kindle-notes-web-app)

(defmethod print-object ((web-app-user web-app-user) stream)
  "Print web app user."
  (call-next-method)
  (print-unreadable-object (web-app-user stream :type t)
    (with-accessors ((user-name user-name)) web-app-user
      (format stream
       	      "User Name: ~A" user-name))))

;; TODO - inherit from application-user to add name field + persisting that additional info
(defun add-user (user-name user-login user-password)
  "Add new user."
  (jfh-app-core:save-new-application-user (make-web-app-user user-name user-login user-password) (jfh-app-core:application-configuration *web-configuration*)))

(defun make-web-app-user (user-name user-login user-password)
  "Constructor for web-app-user."
  (make-instance 'web-app-user :user-name user-name :user-login user-login :user-password user-password))

(defmethod jfh-app-core:save-application-user ((web-app-user web-app-user) (application-configuration jfh-app-core:application-configuration))
  "Input: web-app-user and app-configuration. Output: serialized web-app-user (sub-class specific fields only) . Persist application user info."
  (call-next-method)
  (let ((file-name "web-app-user.sexp")
        (user-info-list (list
                         :user-name (user-name web-app-user))))
    (jfh-app-core:save-user file-name user-info-list web-app-user application-configuration)))
