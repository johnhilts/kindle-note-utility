;;;; functions related to an application user.
(cl:in-package #:jfh-app-core)

(defmethod initialize-instance :after ((application-user application-user) &key)
  "Initializations:
- Encrypt the user password. This is meant to prevent the plain text password from being in memory.
- Set the User ID to a unique ID."
  (let ((user-id #1=(slot-value application-user '%user-id))
        (password #2=(slot-value application-user '%user-password)))
    (when (zerop (length user-id))
      (setf #1# (jfh-utility:generate-unique-token)))
    (when (zerop (length password))
      (setf #2# (jfh-utility:hash-password password)))))

(defun make-application-user (user-name user-password)
  "Constructor for application-user."
  (make-instance 'application-user :user-name user-name :user-password user-password))

(defmethod find-user-path ((application-user application-user) (application-configuration application-configuration))
  "Input: application-user and app-configuration. Output: user path."
  (with-accessors ((user-path-root user-path-root)) application-configuration
    (with-accessors ((user-id user-id)) application-user
      (format nil "~A~A/" user-path-root user-id))))

(defmethod save-application-user ((application-user application-user) (application-configuration application-configuration))
  "Input: application-user and app-configuration. Output: application-user. Persist application user info."
  (let ((user-info-file-path (format nil "~Auser.sexp" (find-user-path application-user application-configuration)))
        (user-info-list (list
                         :user-id (user-id application-user)
                         :user-name (user-name application-user)
                         :user-password (user-password application-user)
                         :create-date (create-date application-user)
                         :disable (disable application-user))))
    (jfh-utility:write-complete-file user-info-file-path user-info-list)))

(defmethod save-new-application-user ((application-user application-user) (application-configuration application-configuration))
  "Input: application-user and app-configuration. Output: application-user. Persist application user info."
  (let* ((user-path-root (user-path-root application-configuration))
         (user-index-file-path (format nil "~Auser-index.sexp" user-path-root)))
    (flet ((callback (user-index)
             (push (cons (user-name application-user) (user-id application-user)) user-index)
             (jfh-utility:write-complete-file user-index-file-path user-index)))
      (ensure-directories-exist user-path-root)
      (jfh-utility:fetch-or-create-data user-index-file-path #'callback)
      (ensure-directories-exist (find-user-path application-user application-configuration))
      (save-application-user application-user application-configuration))))
