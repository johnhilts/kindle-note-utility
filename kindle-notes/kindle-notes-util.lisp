(in-package #:jfh-kindle-notes-util)

(defun list-titles (&optional search (notes jfh-kindle-notes::*note-headers*) is-html)
  "Output a list of titles."
  (let ((titles (sort
		 (copy-list
		  (remove-duplicates
		   (map 'list
			(lambda (note) (cl-ppcre:regex-replace-all "﻿" (jfh-kindle-notes::title note) ""))
			(if search
			    (remove-if-not (lambda (note) (search search (jfh-kindle-notes::title note) :test #'string-equal)) notes)
			    notes))
		   :test #'string-equal))
		 #'string-lessp)))
    (if is-html
	titles
	(format nil "~{~A~^~%~}" titles))))

(defun search-notes (notes search &key in (formatter #'identity) (format "~{~A~^~%~}"))
  "Search for matching text. Inputs: search string to match text, and option :in to match titles in a list. Sub-string matches are acceptable for both inputs."
  (format nil format
          (loop for kindle-entry across
                                 (remove-if-not
                                  (lambda (note) (search search (jfh-kindle-notes::text note) :test #'string-equal))
                                  (remove-if-not
                                   (lambda (note) (if in (some (lambda (in-entry) (search in-entry (jfh-kindle-notes::title note) :test #'string-equal)) in) note))
                                   notes))
                collect
                (funcall formatter (jfh-kindle-notes::format-object kindle-entry)))))
