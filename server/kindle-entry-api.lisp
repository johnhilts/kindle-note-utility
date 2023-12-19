;;;; Web API for kindle entries
(cl:in-package #:jfh-kindle-notes-web-app)

(define-api-endpoint daily-tip "/daily-tip" ()
  "REST endpoint for tip of the day"
  (case verb
    (:get
     (jfh-kindle-notes-server-infrastructure:serialize-to-json (jfh-kindle-notes:show-tip-of-the-day)))))
