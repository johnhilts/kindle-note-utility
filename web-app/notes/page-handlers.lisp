;;;; Web pages for kindle entries
(cl:in-package #:jfh-kindle-notes-web-app)

(auth:define-protected-page (daily-tip "/daily-tip") ()
  "daily tip page"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Daily Tip from your Kindle Notes"))
     (:body
      (:div
       (who:str (format-for-web (jfh-kindle-notes:format-object (jfh-kindle-notes:show-tip-of-the-day (gethash auth:authenticated-user *notes*))))))))))

(auth:define-protected-page (daily-tip-simple "/daily-tip-simple") ()
  "daily tip page (simple string only version no markup)"
  (jfh-kindle-notes:format-object (jfh-kindle-notes:show-tip-of-the-day)))

(auth:define-protected-page (search-handler "/search") ()
  "search page"
  (let((title-id-prefix "title"))
    (flet ((get-title-checked-from-request ()
             (remove-if-not
              (lambda (e)
                (jfh-utility:string-starts-with title-id-prefix (car e)))
              (tbnl:post-parameters tbnl:*request*))))
      (let ((query (tbnl:post-parameter "query")))
        (who:with-html-output-to-string
	    (*standard-output* nil :prologue t :indent t)
          (:html
           (who:str (common-header "Search your Kindle Notes"))
           (:body
	    (:div
	     (:form :method "post" :action "search"
		    (:div
		     (:div (:textarea :id "query" :name "query" :placeholder "Write text to search on here" :autofocus "autofocus" (who:str (if query query ""))))
		     (:div (:button "Search"))
		     (when query
		       (let* ((in (mapcar 'cdr (get-title-checked-from-request)))
			      (results (jfh-kindle-notes-util:search-notes (gethash auth:authenticated-user *notes*) query :in in :formatter #'format-for-web :format "~{~A<hr />~}")))
		         (when results
		           (who:htm
			    (:span (who:str results)))))))
		    (let ((titles (jfh-kindle-notes-util:list-titles nil (gethash auth:authenticated-user *notes*) t))
		          (checked (mapcar 'car (get-title-checked-from-request))))
		      (who:htm
		       (:div
		        (loop for title in titles
			      for i = 1 then (incf i)
			      for title-id = (format nil "~A~D" title-id-prefix i)
			      do
			         (who:htm
			          (:div
			           (who:htm
                                    (:input :type "checkbox" :id title-id :name title-id :value (who:str title)
                                            :checked (if (find title-id checked :test #'string=) t nil)))
			           (:label :for title-id (who:str title))))))))))))))))))

(auth:define-protected-page (upload-list-handler "/upload-list") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Daily Tip from your Kindle Notes"))
     (:body
      (:div
       (who:str (upload-list auth:authenticated-user)))))))

(auth:define-protected-page (uploaded-titles-handler "/uploaded-titles") ()
  (uploaded-titles auth:authenticated-user))

(auth:define-protected-page (upload-handler "/upload") ()
  (upload auth:authenticated-user))
