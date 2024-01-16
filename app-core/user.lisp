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

(defmethod print-object ((application-user application-user) stream)
  "Print application user."
  (print-unreadable-object (application-user stream :type t)
    (with-accessors ((user-id user-id) (user-name user-name) (create-date create-date) (disable disable)) application-user
      (format stream
	      "User ID: ~A, User Name: ~S, Created: ~A, Disabled: ~:[false~;true~]" user-id user-name create-date disable))))

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

(defmethod print-object ((user-index-entry user-index-entry) stream)
  "Print user index entry."
  (print-unreadable-object (user-index-entry stream :type t)
    (with-accessors ((user-id user-id) (user-name user-name)) user-index-entry
      (format stream
	      "User ID: ~A, User Name: ~S" user-id user-name))))

(defmethod make-user-index-entry ((application-user application-user))
  "Input: application-user. Output: user index entry."
  (make-instance 'user-index-entry
		 :user-name (user-name application-user)
		 :user-id (user-id application-user)))

(defmethod user-index-entry->list ((user-index-entry user-index-entry))
  "Input: user index entry. Output: regular list. Conversion function."
  (list
   :user-id (user-id user-index-entry)
   :user-name (user-name user-index-entry)))

(defmethod save-new-application-user ((application-user application-user) (application-configuration application-configuration))
  "Input: application-user and app-configuration. Output: application-user. Persist application user info."
  (let* ((user-path-root (user-path-root application-configuration))
         (user-index-file-path (format nil "~Auser-index.sexp" user-path-root)))
    (flet ((callback (user-index)
             (push (user-index-entry->list (make-user-index-entry application-user)) user-index)
             (jfh-utility:write-complete-file user-index-file-path user-index)))
      (ensure-directories-exist user-path-root)
      (jfh-utility:fetch-or-create-data user-index-file-path #'callback)
      (ensure-directories-exist (find-user-path application-user application-configuration))
      (save-application-user application-user application-configuration))))

;; TODO convert this function to FIND-USER-INFO
;; notes on differences in user index file
;; 1. saving as p-list
;; 2. keys are also being saved
;; 3. order is switched from todo app
;; 4. examples
;; (getf (user-index-entry->list (make-user-index-entry *jfh-user*)) :user-name)
;; (getf (user-index-entry->list (make-user-index-entry *jfh-user*)) :user-id)
(defun find-user-info (user-name)
  "Search for user info in file system."
  (let* ((user-index-entry (find-user-index-entry search-value :by by))
         (user-guid (cadr user-index-entry)))
    (awhen user-guid
      (read-user-info it))))

;; TODO convert this function to fit with this app
(defun find-user-index-entry (user-id)
  "Search for user info by specifified field in user index file."
  (let ((user-index (or *user-index* (read-user-index))))
    (case by
      (:login (find search-value user-index :test #'(lambda (search-value e) (string= search-value (car e)))))
      (:guid (find search-value user-index :test #'(lambda (search-value e) (string= search-value (cadr e))))))))

#|
./source/kindle/kindle-note-utility/app-core/user.lisp:45:      (ensure-directories-exist (FIND-USER-PATH application-user application-configuration))
./source/kindle/kindle-note-utility/app-core/user.lisp:53:     (let* ((user-index-entry (FIND-USER-INDEX-ENTRY search-value :by by))
./source/kindle/kindle-note-utility/web-auth/web-auth.lisp:26:           (funcall (FIND-USER-INFO *web-auth-pages*) user-name)))
./source/kindle/kindle-note-utility/web-auth/pages.lisp:5:  (let ((user-info (funcall (FIND-USER-INFO *web-auth-pages*) user :by :login)))
./source/kindle/kindle-note-utility/web-app/auth.lisp:62:                  (let ((user-info (FIND-USER-ENTRY (tbnl:post-parameter "user") :by :login)))
|#
