;;;; Web pages for kindle entries (uploads)
(cl:in-package #:jfh-kindle-notes-web-app)

(defun handle-file (post-parameter)
  (when (and post-parameter
             (listp post-parameter))
    (destructuring-bind (path file-name content-type)
        post-parameter
      (let ((new-path (make-pathname :name (format nil "hunchentoot-test-~A"
                                                   (random 100))
                                     :type nil
                                     :defaults #p"/tmp/hunchentoot/test/")))
        ;; strip directory info sent by Windows browsers
        (when (search "Windows" (tbnl:user-agent) :test 'char-equal)
          (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
        (rename-file path (ensure-directories-exist new-path))
        (list new-path file-name content-type)))))

(defun get-uploads ()
  (list 1))

(defun upload ()
  "html page to facilitate uploads."
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
