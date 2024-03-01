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
      (if (get-uploads)
          (who:htm
           (:div "Get Started")
           (:div
            (:a :href "/upload-list" "Your Uploads"))
           (:div
            (:a :href "/upload" "Upload More"))
           (:div
            (:a :href "/daily-tip" "Daily Tip from your Kindle Notes.")))
          (who:htm
           (:div
            (:a :href "/upload" "Get Started"))
           (:div "Your Uploads")
           (:div "Daily Tip from your Kindle Notes.")))))))

(auth:define-protected-page (daily-tip "/daily-tip") ()
  "daily tip page"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Daily Tip from your Kindle Notes"))
     (:body
      (:div
       (who:str (jfh-kindle-notes:format-object (jfh-kindle-notes:show-tip-of-the-day (gethash auth:authenticated-user *notes*)))))))))

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

(auth:define-protected-page (upload-list-handler "/upload-list") ()
  (upload-list))

(auth:define-protected-page (upload-handler "/upload") ()
  (upload auth:authenticated-user))

(tbnl:define-easy-handler (upload-test-handler :uri "/upload-test") ()
  (upload-test))
