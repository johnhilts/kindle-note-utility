;;;; Web API for kindle entries
(cl:in-package #:jfh-kindle-notes-web-app)

(jfh-web-core:define-api-endpoint daily-tip-data "/daily-tip-data" ()
  "REST endpoint for tip of the day"
  (case jfh-web-core:verb
    (:get
     (jfh-utility:serialize-to-json (jfh-kindle-notes:show-tip-of-the-day)))))
