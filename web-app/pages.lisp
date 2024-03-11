;;;; Web pages for kindle entries
(cl:in-package #:jfh-kindle-notes-web-app)

(defun get-version () "1")

(tbnl:define-easy-handler (root :URI "/") ()
  "root route handler"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Daily Tip from your Kindle Notes"))
     (:body
      (:div
       "Welcome to the kindle notes utility!")
      (:div :style "float:right"
	    (:a :href "/logout" "Logout"))
      (:div "&nbsp;")
      (multiple-value-bind (authenticated-user-id present-p)
          (auth:get-authenticated-user)
	(if (and present-p
		 (get-uploaded-file-index authenticated-user-id))
            (who:htm
             (:div "Get Started")
             (:div
              (:a :href "/upload-list" "Your Uploads"))
             (:div
              (:a :href "/upload" "Upload More"))
             (:div
              (:a :href "/daily-tip" "Daily Tip from your Kindle Notes."))
	     (:div
              (:a :href "/search" "Search.")))
            (who:htm
             (:div
              (:a :href "/upload" "Get Started"))
             (:div "Your Uploads")
             (:div "Daily Tip from your Kindle Notes."))))))))

(defun format-for-web (string)
  "Replace line-breaks \n with the HTML <br /> element."
  (cl-ppcre:regex-replace-all (string #\Newline) string "<br />"))

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
			      (results (jfh-kindle-notes-util:search-notes (gethash auth:authenticated-user *notes*) query :in in)))
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
			           (if  (find title-id checked :test #'string=) ;; todo this sucks see if we can use something like `class="check"` instead ...
			                (who:htm (:input :type "checkbox" :id title-id :name title-id :value (who:str title) :checked "checked"))
			                (who:htm (:input :type "checkbox" :id title-id :name title-id :value (who:str title))))
			           (:label :for title-id (who:str title))))))))))))))))))

(auth:define-protected-page (admin-page "/admin") ()
  (let ((web-user (find-web-user-info auth:authenticated-user)))
    (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Admin"))
     (:body
      (:h2 (who:fmt "Welcome to the Admin Page, ~A!" (user-name web-user)))
      (:div "You're supposed to be logged in to see this!")
      (:div
       (:a :href "/logout" "Click here to logout!")))))))


(tbnl:define-easy-handler (version-page :uri "/version") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Version"))
     (:body
      (:div "Version")
      (:div (who:str(get-version)))))))

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
