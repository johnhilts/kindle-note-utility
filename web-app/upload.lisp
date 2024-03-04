;;;; Web pages for kindle entries (uploads)
(cl:in-package #:jfh-kindle-notes-web-app)

(defun process-upload-file (post-parameter authenticated-user-id)
  "Handles file uploads."
  (when (and post-parameter
             (listp post-parameter))
    (destructuring-bind (path file-name content-type)
        post-parameter
      ;; strip directory info sent by Windows browsers
      (when (search "Windows" (tbnl:user-agent) :test 'char-equal)
        (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
      (multiple-value-bind
	    (new-path new-file-name)
	  (get-notes-path authenticated-user-id)
	(rename-file path (ensure-directories-exist new-path))
	(read-user-notes authenticated-user-id new-path)
	(list new-path new-file-name content-type)))))

(defun get-uploads ()
  ())

(defun upload (authenticated-user-id)
  "html page to facilitate uploads."
  (let ((file-uploaded nil))
    (when (tbnl:post-parameter "upload-file")
      (setf file-uploaded (process-upload-file (tbnl:post-parameter "upload-file") authenticated-user-id)))
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
	  (who:str
	   (upload-list file-uploaded))))))))

(defun upload-list (&optional file-uploaded)
  "html page to list and manage uploads."
  (who:with-html-output-to-string (*standard-output* nil :prologue nil)
    (who:htm
     (:p
      (:table :border 1 :cellpadding 2 :cellspacing 0
              (:tr (:td :colspan 2 (:b "Uploaded files")))
	      (when file-uploaded
		(destructuring-bind (path file-name content-type) file-uploaded
		  (who:htm
		   (:tr
                    (:td (:a :href (format nil "files/~A?path=~A" (tbnl:url-encode file-name) (tbnl:url-encode (namestring path)))
                             (who:esc file-name)))
                    (:td :align "right"
			 (who:str (ignore-errors
				   (with-open-file (in path)
                                     (file-length in))))
			 (who:str (format nil "&nbsp;Bytes (type: ~A)" content-type)))))))
	      (let ((uploaded-files ())) ;; todo get uploaded-files from a user file that keeps (list new-path file-name content-type) for each uploaded file
		(loop for (path file-name content-type) in uploaded-files
		      do
			 (who:htm
			  (:tr 
			   (:td (:a :href (format nil "files/~A?path=~A" (tbnl:url-encode file-name) (tbnl:url-encode (namestring path)))
				    (who:esc file-name)))
			   (:td :align "right"
				(who:str (ignore-errors
					  (with-open-file (in path)
					    (file-length in))))
				(who:str (format nil "&nbsp;Bytes (type: ~A)" content-type)))))))
	                    (:tr (:td :colspan 2 (:b (:a :href "/daily-tip" "See Daily Tip")))))))))
