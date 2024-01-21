;;;; functions related to an application user.
(cl:in-package #:jfh-app-core)

(defmethod initialize-instance :after ((application-user application-user) &key)
  "Initializations:
- Encrypt the user password. This is meant to prevent the plain text password from being in memory.
- Set the User ID to a unique ID."
  (let ((user-id #1=(slot-value application-user '%user-id))
        (password #2=(slot-value application-user '%user-password))
        (create-date #3=(slot-value application-user '%create-date)))
    (when (zerop (length user-id))
      (setf #1# (jfh-utility:generate-unique-token))
      (setf #2# (jfh-utility:hash-password password))
      (setf #3# (get-universal-time)))))

(defun make-application-user (user-login user-password)
  "Constructor for application-user."
  (make-instance 'application-user :user-login user-login :user-password user-password))

(defun make-minimum-application-user (user-id)
  "Constructor for application-user."
  (make-instance 'application-user :user-id user-id :user-login ""))

(defmethod print-object ((application-user application-user) stream)
  "Print application user."
  (print-unreadable-object (application-user stream :type t)
    (with-accessors ((user-id user-id) (user-login user-login) (create-date create-date) (disable disable)) application-user
      (format stream
	      "User ID: ~A, User Login: ~S, Created: ~A, Disabled: ~:[false~;true~]" user-id user-login create-date disable))))

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
                         :user-login (user-login application-user)
                         :user-password (user-password application-user)
                         :create-date (create-date application-user)
                         :disable (disable application-user))))
    (jfh-utility:write-complete-file user-info-file-path user-info-list)))

(defmethod print-object ((user-index-entry user-index-entry) stream)
  "Print user index entry."
  (print-unreadable-object (user-index-entry stream :type t)
    (with-accessors ((user-id user-id) (user-login user-login)) user-index-entry
      (format stream
	      "User ID: ~A, User Name: ~S" user-id user-login))))

(defmethod make-user-index-entry ((application-user application-user))
  "Input: application-user. Output: user index entry."
  (make-instance 'user-index-entry
		 :user-login (user-login application-user)
		 :user-id (user-id application-user)))

(defmethod user-index-entry->list ((user-index-entry user-index-entry))
  "Input: user index entry. Output: regular list. Conversion function."
  (list
   :user-id (user-id user-index-entry)
   :user-login (user-login user-index-entry)))

(defun get-user-index-file-path (user-path-root)
  (format nil "~Auser-index.sexp" user-path-root))

(defmethod save-new-application-user ((application-user application-user) (application-configuration application-configuration))
  "Input: application-user and app-configuration. Output: application-user. Persist application user info."
  (let* ((user-path-root (user-path-root application-configuration))
         (user-index-file-path (get-user-index-file-path user-path-root)))
    (flet ((callback (user-index)
             (push (user-index-entry->list (make-user-index-entry application-user)) user-index)
             (jfh-utility:write-complete-file user-index-file-path user-index)))
      (ensure-directories-exist user-path-root)
      (jfh-utility:fetch-or-create-data user-index-file-path #'callback)
      (ensure-directories-exist (find-user-path application-user application-configuration))
      (save-application-user application-user application-configuration))))

(defun user-entry->application-user (user-entry)
  (make-instance 'application-user
		 :user-id (getf user-entry :user-id)
		 :user-login (getf user-entry :user-login)
		 :user-password (getf user-entry :user-password)
		 :create-date (getf user-entry :create-date)
		 :disable (getf user-entry :disable)))

(defun read-user-info (user-id)
  "read user info from user-id/user.sexp The guid-like user ID is needed to find the folder."
  (let ((minimum-user-info (make-minimum-application-user user-id)))
    (jfh-utility:read-complete-file (format nil "~A/user.sexp" (find-user-path minimum-user-info (make-application-configuration))))))

;; TODO convert this function to FIND-USER-INFO
;; notes on differences in user index file
;; 1. saving as p-list
;; 2. keys are also being saved
;; 3. order is switched from todo app
;; 4. examples
;; (getf (user-index-entry->list (make-user-index-entry *jfh-user*)) :user-name)
;; (getf (user-index-entry->list (make-user-index-entry *jfh-user*)) :user-id)
#|
(let ((list (list
	       (list :USER-NAME "them@somewhere.com" :USER-ID "890-321")
	       (list :USER-NAME "you@there.com" :USER-ID "123-457")
	       (user-index-entry->list (make-user-index-entry *jfh-user*)))))
		(getf (find-if (lambda (entry) (string= (getf entry :user-name) "me@here.com")) list) :user-id)) ; => "2244336-27999-7839125-96272"
|#
(defun find-user-info (user-login)
  "Search for user info in file system."
  (let* ((user-index-entry (find-user-index-entry user-login (make-application-configuration)))
         (user-id (getf user-index-entry :user-id)))
    (when user-id
      (user-entry->application-user (read-user-info user-id)))))

(defmethod find-user-index-entry (user-login (application-configuration application-configuration))
  "Input: User ID and app-configuration. Output: user index entry."
  (let* ((user-path-root (user-path-root application-configuration))
         (user-index-file-path (get-user-index-file-path user-path-root))
	 (user-index (jfh-utility:fetch-or-create-data user-index-file-path)))
    (find-if (lambda (entry) (string= (getf entry :user-login) user-login)) user-index)))

#|
./source/kindle/kindle-note-utility/app-core/user.lisp:45:      (ensure-directories-exist (FIND-USER-PATH application-user application-configuration))
./source/kindle/kindle-note-utility/app-core/user.lisp:53:     (let* ((user-index-entry (FIND-USER-INDEX-ENTRY search-value :by by))
./source/kindle/kindle-note-utility/web-auth/web-auth.lisp:26:           (funcall (FIND-USER-INFO *web-auth-pages*) user-name)))
./source/kindle/kindle-note-utility/web-auth/pages.lisp:5:  (let ((user-info (funcall (FIND-USER-INFO *web-auth-pages*) user :by :login)))
./source/kindle/kindle-note-utility/web-app/auth.lisp:62:                  (let ((user-info (FIND-USER-ENTRY (tbnl:post-parameter "user") :by :login)))
|#
