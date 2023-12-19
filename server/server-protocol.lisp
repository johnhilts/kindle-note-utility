;;;; JSON (de-)serialization, file I/O
(cl:in-package #:jfh-kindle-notes-server-infrastructure)

(defgeneric serialize-to-json (jfh-kindle-notes:kindle-entry)
  (:documentation "Input: kindle-entry object. Output: JSON object."))
;; (documentation 'serialize-to-json 'function)
