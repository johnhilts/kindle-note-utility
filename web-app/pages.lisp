;;;; Web pages for kindle entries
(cl:in-package #:jfh-kindle-notes-web-app)

(defun get-version () "1")

(tbnl:define-easy-handler (root :URI "/") ()
  "root route handler"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Daily Tip from your Kindle Notes"))
     (:body
      (:div
       "Welcome to the kindle notes utility!")
      (:div "&nbsp;")
      (:div
       (:a :href "/daily-tip" "Daily Tip from your Kindle Notes."))))))

(auth:define-protected-page (daily-tip "/daily-tip") ()
  "daily tip page"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Daily Tip from your Kindle Notes"))
     (:body
      (:div
       (who:str (jfh-kindle-notes:format-object (jfh-kindle-notes:show-tip-of-the-day))))))))

(auth:define-protected-page (daily-tip-simple "/daily-tip-simple") ()
  "daily tip page (simple string only version no markup)"
  (jfh-kindle-notes:format-object (jfh-kindle-notes:show-tip-of-the-day)))

(auth:define-protected-page (admin-page "/admin") ()
  (let ((web-user (find-web-user-info auth:authenticated-user)))
    (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Admin"))
     (:body
      (:h2 (who:fmt "Welcome to the Admin Page, ~A!" (user-name web-user)))
      (:div "You're supposed to be logged in to see this!")
      (:div
       (:a :href "/logout" "Click here to logout!")))))))


(tbnl:define-easy-handler (version-page :uri "/version") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Version"))
     (:body
      (:div "Version")
      (:div (who:str(get-version)))))))

(defun handle-file (post-parameter)
  (when (and post-parameter
             (listp post-parameter))
    (destructuring-bind (path file-name content-type)
        post-parameter
      (let ((new-path (make-pathname :name (format nil "hunchentoot-test-~A"
                                                   (random 100))
                                     :type nil
                                     :defaults #p"/tmp/hunchentoot/test/")))
        ;; strip directory info sent by Windows browsers
        (when (search "Windows" (tbnl:user-agent) :test 'char-equal)
          (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
        (rename-file path (ensure-directories-exist new-path))
        (list new-path file-name content-type)))))

(defun upload-test ()
  (let ((file-uploaded nil))
    (when (tbnl:post-parameter "file1")
      (setf file-uploaded (handle-file (tbnl:post-parameter "file1"))))
    (tbnl:no-cache)
    (who:with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (who:str (common-header "Hunchentoot file upload test"))
       (:body
        (:h2 "File Upload Test")
        (:form :method :post :enctype "multipart/form-data"
               (:p "File: "
                   (:input :type :file :name "file1"))
               (:p
                (:input :type :submit)))
        (when file-uploaded
          (destructuring-bind (path file-name content-type) file-uploaded
            (who:htm
             (:p
              (:table :border 1 :cellpadding 2 :cellspacing 0
                      (:tr (:td :colspan 3 (:b "Uploaded files")))
                      (who:htm
                       (:tr (:td :align "right")
                            (:td (:a :href (format nil "files/~A?path=~A" (tbnl:url-encode file-name) (tbnl:url-encode (namestring path)))
                                     (who:esc file-name)))
                            (:td :align "right"
                                 (who:str (ignore-errors
                                           (with-open-file (in path)
                                             (file-length in))))
                                 (who:str (format nil "&nbsp;Bytes (type: ~A)" content-type)))))))))))))))

(tbnl:define-easy-handler (upload-test-handler :uri "/upload-test") ()
  (upload-test))
