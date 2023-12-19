;;;; JSON (de-)serialization
(cl:in-package #:jfh-kindle-notes-server-infrastructure)

(defmethod serialize-to-json ((kindle-entry jfh-kindle-notes:kindle-entry))
  "Input: kindle-entry object. Output: JSON object."
  (cl-json:encode-json-to-string kindle-entry))
