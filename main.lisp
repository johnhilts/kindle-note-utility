(in-package #:jfh-kindle-notes-main)

(defun register-web-auth-functions ()
  "Register functions using provided class to enable web-auth features."
  (make-instance 'auth:web-auth-pages
                 :on-auth-hook 'web-app:on-auth-hook
		 :signup-page 'web-app:signup-page
		 :login-page 'web-app:login-page
		 :find-user-info 'jfh-app-core:find-user-info
		 :find-secure-user-info 'jfh-app-core:find-secure-user-info
		 :show-auth-failure 'web-app:show-auth-failure))

(defparameter *web-application* nil)
(defparameter *actual-swank-port* nil)

(defun application-start ()
  "Use this to start the application."
  (flet ((map-static-paths ()
	   (mapc
	    (lambda (pair)
	      (jfh-web-core:add-static-path-map (car pair) (cadr pair)))
	    jfh-kindle-notes-web-app:*static-paths-maps*)))

    (map-static-paths)
    
    (let ((web-application (jfh-web-core:web-application-shell)))

      (setf web-app:*web-configuration* (web:web-configuration web-application))
      (auth:use-web-auth (register-web-auth-functions))
      
      (setf *actual-swank-port* (jfh-app-core:start-swank (jfh-app-core:application-configuration (jfh-web-core:web-configuration web-application))))

      (setf *web-application* web-application))))

(defun application-stop (&optional (stop-swank t) (web-application *web-application*) (actual-swank-port *actual-swank-port*))
  "Use this to stop the application. Stopping swank is optional."
  (jfh-web-core:stop-web-app web-application)
  (if (and stop-swank actual-swank-port)
      (jfh-app-core:stop-swank (jfh-app-core:application-configuration web-app:*web-configuration*))))

(defun %refresh-web-auth-functions ()
  "The function pointers don't automatically updated when a function is re-compiled, so use this to update them."
  (auth:use-web-auth (register-web-auth-functions)))

(defun %current-swank-port ()
  "Get the current swank port used by the web app."
  (jfh-app-core:swank-port (jfh-app-core:application-configuration web-app:*web-configuration*)))

(defun %list-all-package-symbols (package-name)
  (with-output-to-string (s)
    (with-package-iterator (next-symbol (list-all-packages)
					:internal)
      (loop
	(multiple-value-bind (more? symbol) (next-symbol)
	  (if more? 
	      (when (string-equal (package-name (symbol-package symbol)) "JFH-KINDLE-NOTES") 
		(format s "Symbol: ~A, Package: ~A~%" symbol (package-name (symbol-package symbol))))
	      (return)))))))

(defun %list-all-external-symbols (&optional (package-list (list 'jfh-kindle-notes 'jfh-kindle-notes-web-app 'jfh-web-core 'jfh-app-core 'jfh-utility)))
  "List external symbols for given packages. Input: list of packages. Output: string."
  (with-output-to-string (string)
    (let ((list))
      (loop for package-name in package-list
	    do
	       (format string "************************~%~A~%************************~%" package-name)
	       (do-external-symbols (symbol (find-package package-name) list)
		 (push symbol list))
	       (format string "~{~A~%~}" (sort (copy-list list) #'string<))))
    string))

(defun %list-all-external-symbols (&optional (package-list (list 'jfh-kindle-notes-main 'jfh-kindle-notes 'jfh-kindle-notes-web-app 'jfh-web-core 'jfh-app-core 'jfh-utility)))
  "List external symbols for given packages. Input: list of packages. Output: tree."
  (let ((list (list)))
    (loop for package-name in package-list
	  for packages = () then ()
	  for symbols = () then ()
	  for functions = () then ()
	  for macros = () then ()
	  for classes = () then ()
	  for generic-functions = () then ()
	  do
	     (do-external-symbols (symbol (find-package package-name) symbols)
	       (cond ((macro-function symbol)
		      (push (symbol-name symbol) macros))
		     ((fboundp symbol)
		      (if (eql 'standard-generic-function (type-of (symbol-function symbol)))
			  (push (list (symbol-name symbol) (swank-backend:arglist symbol)) generic-functions)
			  (push (list (symbol-name symbol) (swank-backend:arglist symbol)) functions)))
		     ((find-class symbol nil)
		      (push (symbol-name symbol) classes))
		     (t
		      (push (symbol-name symbol) symbols))))
	     (push
	      (nconc packages
		     (list package-name
			   (nconc
			    (if generic-functions (list :generic-functions (sort generic-functions 'string< :key 'car)) nil)
			    (if classes (list :classes (sort classes 'string<)) nil)
			    (if functions (list :functions (sort functions 'string< :key 'car)) nil)
			    (if macros (list :macros (sort macros 'string<)) nil)
			    (if symbols (list :symbols (sort symbols 'string<)) nil))))
	      list))
    list))

;; TODO - add call to instantiate web-auth instance
;; (auth:use-web-auth (make-instance 'auth:web-auth-pages)) [need to fill in actual functions!!]
;; (funcall (function auth:*web-auth-pages*))
