;;;; Notes
(cl:in-package #:jfh-kindle-notes-web-app)

(defparameter *notes* (make-hash-table)
  "Key: User ID, Value: sequence of user's notes.")

(defun read-user-notes (user-id user-notes-path)
  "Fetch user's notes and save in global dictionary."
  (let ((notes (jfh-kindle-notes:fetch-notes user-notes-path)))
    (setf (gethash user-id *notes*) notes)))
