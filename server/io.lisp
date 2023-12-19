(in-package #:jfh-kindle-notes-server-infrastructure)

(defun read-complete-file (path)
  "read complete file all at once"
  (with-open-file (in path :if-does-not-exist :create)
    (read in nil)))
