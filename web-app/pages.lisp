;;;; Web pages for kindle entries
(cl:in-package #:jfh-kindle-notes-web-app)

(tbnl:define-easy-handler (root :URI "/") ()
  "root route handler"
  "Welcome to the kindle notes utility!")

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
