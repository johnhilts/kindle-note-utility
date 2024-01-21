;;;; functions to support web app users.
(cl:in-package #:jfh-kindle-notes-web-app)

(defmethod print-object ((web-app-user web-app-user) stream)
  "Print web app user."
  (call-next-method)
  (print-unreadable-object (web-app-user stream :type t)
    (with-accessors ((user-name user-name)) web-app-user
      (format stream
       	      "User Name: ~A" user-name))))
