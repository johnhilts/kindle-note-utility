;;;; Configure settings for this web app
(cl:in-package #:jfh-kindle-notes-web-app)

(defparameter *static-paths-maps*
  '(("/favicon.ico" "ez-favicon.ico")
    ("/styles.css" "static/styles.css")))
