;;;; protocol related to general application concerns.
(cl:in-package #:jfh-app-core)

(defclass application-configuration ()
  ((%swank-port :reader swank-port
		:initarg :swank-port)
   (%swank-interface :reader swank-interface
		     :initarg :swank-interface)
   (%settings-file-path :reader settings-file-path
			:initarg :settings-file-path)
   (%user-path-root :reader user-path-root
                    :initarg :user-path-root
                    :initform (error "Value required for :user-path-root")))
  (:documentation "Application configurations."))

(defgeneric start-swank (application-configuration)
  (:documentation "Input: application-configuration. Start swank with the provided configuration settings."))
 
(defgeneric stop-swank (application-configuration)
  (:documentation "Input: application-configuration. Stop swank with the provided configuration settings."))

(defclass application-user ()
  ((%user-id :reader user-id
	     :initarg :user-id
	     :initform "")
   (%user-name :reader user-name
	       :initarg :user-name)
   (%user-password :reader user-password
		   :initarg :user-password
                   :initform "")
   (%create-date :reader create-date
		 :initarg :create-date
		 :initform (get-universal-time))
   (%disable :reader disable
	     :initarg :disable
             :initform nil))
  (:documentation "Application user info."))

(defclass user-index-entry ()
  ((%user-name :reader user-name
	       :initarg :user-name)
   (%user-id :reader user-id
	       :initarg :user-id))
  (:documentation "User index entry. Link User ID to persisted data."))

(defgeneric find-user-path (application-user application-configuration)
  (:documentation "Input: application-user and app-configuration. Output: user path."))

(defgeneric find-user-index-entry (application-user application-configuration)
  (:documentation "Input: application-user and app-configuration. Output: user index entry."))

(defgeneric make-user-index-entry (application-user)
  (:documentation "Input: application-user. Output: user index entry."))

(defgeneric user-index-entry->list (user-index-entry)
  (:documentation "Input: user index entry. Output: regular list. Conversion function."))

(defgeneric save-application-user (application-user application-configuration)
  (:documentation "Input: application-user and app-configuration. Output: application-user. Persist application user info."))

(defgeneric save-new-application-user (application-user application-configuration)
  (:documentation "Input: application-user. Output: application-user. Persist *NEW* application user info."))
