;;;; Web pages for kindle entries
(cl:in-package #:jfh-kindle-notes-web-app)

(defun get-version () "1")

(tbnl:define-easy-handler (root :URI "/") ()
  "root route handler"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title "Home page for \"Daily Tip from your Kindle Notes\"")
      (:link :type "text/css"
             :rel "stylesheet"
             :href (format nil "~A~A~D" (web:static-root jfh-kindle-notes-main:*web-configuration*) "/styles.css?v=" (get-version))))
     (:body
      (:div
       "Welcome to the kindle notes utility!")
      (:div "&nbsp;")
      (:div
       (:a :href "/daily-tip" "Daily Tip from your Kindle Notes."))))))

(tbnl:define-easy-handler (daily-tip :URI "/daily-tip") ()
  "daily tip page"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title "Daily Tip from your Kindle Notes")
      ;; (:link :type "text/css"
      ;;        :rel "stylesheet"
      ;;        :href (format-string  *static-root* "/styles.css?v=" (get-version)))
      )
     (:body
      (:div
       (who:str (jfh-kindle-notes:format-object (jfh-kindle-notes:show-tip-of-the-day))))))))

(tbnl:define-easy-handler (daily-tip-simple :URI "/daily-tip-simple") ()
  "daily tip page (simple string only version no markup)"
  (jfh-kindle-notes:format-object (jfh-kindle-notes:show-tip-of-the-day)))
