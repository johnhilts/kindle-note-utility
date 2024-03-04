(in-package #:jfh-kindle-notes-util)

(defun list-titles (&optional search (notes jfh-kindle-notes::*note-headers*) is-html)
  "Output a list of titles."
  (let ((titles (sort
		 (copy-list
		  (remove-duplicates
		   (map 'list
			(lambda (note) (jfh-kindle-notes::title note))
			(if search
			    (remove-if-not (lambda (note) (search search (jfh-kindle-notes::title note) :test #'string-equal)) notes)
			    notes))
		   :test #'string-equal))
		 #'string<)))
    (if is-html
	titles
	(format nil "~{~A~^~%~}" titles))))

(defun search-for (search &key in)
  "Search for matching text. Inputs: search string to match text, and option :in to match a title. Sub-string matches are acceptable."
  (format nil "~{~A~^~%~}"
          (loop for kindle-entry across
                                 (remove-if-not
                                  (lambda (note) (search search (jfh-kindle-notes::text note) :test #'string-equal))
                                  (remove-if-not
                                   (lambda (note) (if in (search in (jfh-kindle-notes::title note) :test #'string-equal) note))
                                   jfh-kindle-notes::*note-headers*))
                collect
                (with-accessors ((text jfh-kindle-notes::text)
                                 (title jfh-kindle-notes::title)
                                 (location jfh-kindle-notes::location)
                                 (page-number jfh-kindle-notes::page-number))
                    kindle-entry
                  (format nil "From _~A_ (~@[location: ~D~]~@[page number: ~D~]):~%~A~%" (string-trim " " title) location page-number text)))))

(defun print-formatted-date (&optional (universal-time (get-universal-time)) (stream (make-string-output-stream)))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time universal-time)
    (format stream
	    "~D, ~D/~D/~D ~D:~D:~D UTC ~D (ds: ~A)"
	    (case day (0 "Monday") (6 "Sunday"))
	    month
	    date
	    year
	    hour
	    minute
	    second
	    (* -1 zone)
	    (if daylight-p "true" "false"))
    (get-output-stream-string stream)))
