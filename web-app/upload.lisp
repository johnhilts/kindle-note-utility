;;;; Web pages for kindle entries (uploads)
(cl:in-package #:jfh-kindle-notes-web-app)

(defun process-upload-file (post-parameter authenticated-user)
  "Handles file uploads."
  (when (and post-parameter
             (listp post-parameter))
    (destructuring-bind (path file-name content-type)
        post-parameter
      (let* ((user-path (jfh-app-core:find-user-path (find-web-user-info authenticated-user) (jfh-app-core:application-configuration *web-configuration*)))
             (new-file-name "kindle-notes")
             (new-path (make-pathname :name new-file-name
                                      :type "txt"
                                      :defaults (truename user-path))))
        ;; strip directory info sent by Windows browsers
        (when (search "Windows" (tbnl:user-agent) :test 'char-equal)
          (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
        (rename-file path (ensure-directories-exist new-path))
	(read-user-notes new-path)
        (list new-path new-file-name content-type)))))

(defun read-user-notes (user-notes-path)
  "Derive web-user info from app-user."
  (jfh-kindle-notes:refresh-note-headers user-notes-path))

(defun get-uploads ()
  ())

(defun upload (authenticated-user)
  "html page to facilitate uploads."
  (let ((file-uploaded nil))
    (when (tbnl:post-parameter "upload-file")
      (setf file-uploaded (process-upload-file (tbnl:post-parameter "upload-file") authenticated-user)))
    (tbnl:no-cache)
    (who:with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (who:str (common-header "Hunchentoot file upload test"))
       (:body
        (:h2 "File Upload Test")
        (:form :method :post :enctype "multipart/form-data"
               (:p "Select a file with your notes"
                   (:p
                    (:input :type :file :name "upload-file")))
               (:p
                (:input :type :submit :value "Upload")))
        (when file-uploaded
          (destructuring-bind (path file-name content-type) file-uploaded
            (who:htm
             (:p
              (:table :border 1 :cellpadding 2 :cellspacing 0
                      (:tr (:td :colspan 3 (:b "Uploaded files")))
                      (who:htm
                       (:tr (:td :align "right")
                            (:td (:a :href (format nil "files/~A?path=~A" (tbnl:url-encode file-name) (tbnl:url-encode (namestring path)))
                                     (who:esc file-name)))
                            (:td :align "right"
                                 (who:str (ignore-errors
                                           (with-open-file (in path)
                                             (file-length in))))
                                 (who:str (format nil "&nbsp;Bytes (type: ~A)" content-type)))))))))))))))

(defun upload-list ()
  "html page to list and manage uploads."
  (let ((file-uploaded nil))
    (when (tbnl:post-parameter "file1")
      (setf file-uploaded (handle-file (tbnl:post-parameter "file1"))))
    (tbnl:no-cache)
    (who:with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (who:str (common-header "Hunchentoot file upload test"))
       (:body
        (:h2 "File Upload Test")
        (:form :method :post :enctype "multipart/form-data"
               (:p "File: "
                   (:input :type :file :name "file1"))
               (:p
                (:input :type :submit)))
        (when file-uploaded
          (destructuring-bind (path file-name content-type) file-uploaded
            (who:htm
             (:p
              (:table :border 1 :cellpadding 2 :cellspacing 0
                      (:tr (:td :colspan 3 (:b "Uploaded files")))
                      (who:htm
                       (:tr (:td :align "right")
                            (:td (:a :href (format nil "files/~A?path=~A" (tbnl:url-encode file-name) (tbnl:url-encode (namestring path)))
                                     (who:esc file-name)))
                            (:td :align "right"
                                 (who:str (ignore-errors
                                           (with-open-file (in path)
                                             (file-length in))))
                                 (who:str (format nil "&nbsp;Bytes (type: ~A)" content-type)))))))))))))))

(defun upload-test ()
  (let ((file-uploaded nil))
    (when (tbnl:post-parameter "file1")
      (setf file-uploaded (handle-file (tbnl:post-parameter "file1"))))
    (tbnl:no-cache)
    (who:with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (who:str (common-header "Hunchentoot file upload test"))
       (:body
        (:h2 "File Upload Test")
        (:form :method :post :enctype "multipart/form-data"
               (:p "File: "
                   (:input :type :file :name "file1"))
               (:p
                (:input :type :submit)))
        (when file-uploaded
          (destructuring-bind (path file-name content-type) file-uploaded
            (who:htm
             (:p
              (:table :border 1 :cellpadding 2 :cellspacing 0
                      (:tr (:td :colspan 3 (:b "Uploaded files")))
                      (who:htm
                       (:tr (:td :align "right")
                            (:td (:a :href (format nil "files/~A?path=~A" (tbnl:url-encode file-name) (tbnl:url-encode (namestring path)))
                                     (who:esc file-name)))
                            (:td :align "right"
                                 (who:str (ignore-errors
                                           (with-open-file (in path)
                                             (file-length in))))
                                 (who:str (format nil "&nbsp;Bytes (type: ~A)" content-type)))))))))))))))
