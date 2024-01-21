;;;; functions for auth related to this web app; usually called from a page handler
(cl:in-package #:jfh-kindle-notes-web-app)

(defun show-auth-failure ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title "Auth Failure")
      (:link :type "text/css"
             :rel "stylesheet"
             :href (format nil "~A~A~D" (web:static-root jfh-kindle-notes-main:*web-configuration*) "/styles.css?v=" (get-version))))
     (:body
      (:h2 "Authorization failed!")
      (:div "User or password didn't match"
            (:a :href "/login" "Click here to try again!"))))))

(defun login-page (redirect-back-to)
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title "Kindle Notes Utility - Login")
      (:link :type "text/css"
             :rel "stylesheet"
             :href (web:static-root jfh-kindle-notes-main:*web-configuration*)))
     (:body
      (:h2 "Use this page to Login!")
      (:form :method "post" :action "auth"
             (:input :type "hidden" :name "redirect-back-to" :value (or redirect-back-to "/daily-tip"))
             (:div :id "login-input-div"
              (:div (:input :name "user" :type "email" :placeholder "Login" :class "login-input" :autofocus "autofocus"))
              (:div (:input :name "password" :type "password" :placeholder "Password" :class "login-input"))
              (:div (:button "Login") (:span "&nbsp;") (:button :id "sign-up-button" :type "button" :onclick "javascript:location.href=\"/signup\";" "Sign-Up"))))))))

;; TODO - inherit from application-user to add name field + persisting that additional info
(defun add-user (name id password)
  "Add new user."
  (jfh-app-core:save-new-application-user (jfh-app-core:make-application-user id password) (jfh-app-core:application-configuration jfh-kindle-notes-main:*web-configuration*)))

(defun signup-page ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title "Kindle Notes Utility - Signup")
      (:link :type "text/css"
             :rel "stylesheet"
             :href (format nil "~A~A~D" (web:static-root jfh-kindle-notes-main:*web-configuration*) "/styles.css?v=" (get-version))))
     (:body
      (if (or
           (tbnl:post-parameter "name")
           (tbnl:post-parameter "user")
           (tbnl:post-parameter "password")
           (tbnl:post-parameter "confirm-password"))
          (multiple-value-bind (signup-validation-successful signup-validation-failure-reasons)
              (auth:validate-signup-parameters (tbnl:post-parameter "name") (tbnl:post-parameter "user") (tbnl:post-parameter "password") (tbnl:post-parameter "confirm-password"))
            (if signup-validation-successful
                (progn
                  (add-user (tbnl:post-parameter "name") (tbnl:post-parameter "user") (tbnl:post-parameter "password"))
                  (let ((user-info (jfh-app-core:find-user-info (tbnl:post-parameter "user"))))
                    (auth:establish-user-session user-info))
                  (who:htm (:script :type "text/javascript"
                                    (who:str
                                     (ps:ps
                                      (alert "Signup Successful!")
                                      (setf (@ location href) "/daily-tip"))))))
                (who:htm
                 (:div
                  (:span (who:fmt "Signup Failed, because <ul>~{<li>~a</li>~% ~}</ul>" signup-validation-failure-reasons)))
                 (:div
                  (:span "Please try again: ")
                  (:p (:a :href "/signup" "Back to Signup"))
                  (:p (:a :href "/login" "Back to Login"))))))
          (who:htm
           (:h2 "Use this page to sign-up!")
           (:div
            (:a :href "/login" "Back to Login"))
           (:form :method "post" :action "/signup"
                  (:div
                   (:div (:input :name "name" :type "text" :placeholder "Your Name" :class "login-input" :autofocus "autofocus"))
                   (:div (:input :name "user" :type "email" :placeholder "Login" :class "login-input"))
                   (:div (:input :name "password" :type "password" :placeholder "Password" :class "login-input"))
                   (:div (:input :name "confirm-password" :type "password" :placeholder "Confirm Password" :class "login-input"))
                   (:div (:button "Submit"))))))))))

(auth:define-protected-page (admin-page "/admin") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "Admin"))
     (:body
      (:h2 (who:fmt "Welcome to the Admin Page, ~a!" auth:authenticated-user))
      (:div "You're supposed to be logged in to see this!")
      (:div
       (:a :href "/logout" "Click here to logout!"))))))


(tbnl:define-easy-handler (version-page :uri "/version") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "EZ Utils - Version")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href (format-string  *static-root* "/styles.css?v=" (get-version))))
     (:body
      (:div "Version")
      (:div (who:str(get-version)))))))
