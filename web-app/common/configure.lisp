;;;; Configure settings for this web app
(cl:in-package #:jfh-kindle-notes-web-app)

(defvar *web-configuration*)

(defparameter *static-paths-maps*
  '(("/favicon.ico" "ez-favicon.ico")
    ("/styles.css" "static/styles.css")))
