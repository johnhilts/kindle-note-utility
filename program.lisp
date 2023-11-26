(in-package #:cl-user)
(defpackage #:jfh/kindle-notes
  (:use #:common-lisp))

(in-package #:jfh/kindle-notes)

(defparameter *location-marker-1* "位置No. ")

(defparameter *location-marker-2* "位置 No. ")

(defparameter *page-marker* "ページ")

(defclass kindle-entry ()
  ((text
    :initarg :text
    :accessor text
    :documentation "The text of a kindle entry - I think it works like (OR memo highlighted-text)")
   (title
    :initarg :title
    :accessor title
    :documentation "The title of the book where the highlighting took place.")
   (location
    :initarg :location
    :accessor location
    :documentation "The kindle location info of the book where the highlighting took place. I retrieve like this (OR location-info page-number); If this is nil, page-number should be populated.")
   (page-number
    :initarg :page-number
    :accessor page-number
    :documentation "The kindle page number of the book where the highlighting took place. I retrieve like this (OR location-info page-number). This will be nil if location is populated."))
  (:documentation "A kindle note/memo entry. Probably unique, but that's not enforced in code in any way."))

(defmethod print-object ((kindle-entry kindle-entry) stream)
  "Print a kindle entry"
  (print-unreadable-object (kindle-entry stream :type t)
    (with-slots (text title location page-number) kindle-entry
      ;; (format nil "~@[x = ~a ~]~@[y = ~a~]" nil 20)
      (format stream "~s, found in: ~a~@[, location: ~d~]~@[, page number: ~d~]" text title location page-number))))

(defun parse-kindle-entry (stream kindle-entry-text) ;; re-do with regex?
  (flet ((get-location-start-position (line-with-location)
	   (let ((marker-1-position (search *location-marker-1* line-with-location))
		 (marker-2-position (search *location-marker-2* line-with-location)))
	     (when (or
		    marker-1-position
		    marker-2-position)
	       (+
		(if marker-1-position marker-1-position marker-2-position)
		(if marker-1-position (length *location-marker-1*) (length *location-marker-2*))))))
	 (get-page (line-with-location) ;; re-do with regex?
	   (let ((marker-start (search *page-marker* line-with-location))
		 (page-info-start (position-if (lambda (c) (digit-char-p c)) line-with-location)))
	     (if (and
		  marker-start
		  (< page-info-start marker-start))
		 (parse-integer
		  (subseq line-with-location
			  page-info-start
			  (+
			   page-info-start
			   (position-if-not (lambda (c) (digit-char-p c)) line-with-location :start page-info-start)))
		  :junk-allowed t)
		 nil)))
	 (clean (string)
	   (delete #\Return string)))
    (let* ((title (read-line stream nil nil))
	   (line-with-location (read-line stream nil nil))
	   (location-start-position (get-location-start-position line-with-location))
	   (location (if location-start-position
			 (subseq
			  line-with-location
			  location-start-position
			  (+ location-start-position (position-if-not #'digit-char-p (subseq line-with-location location-start-position))))
			 nil))
	   (page-number (if location nil (get-page line-with-location))))
      (make-instance 'kindle-entry :text (clean kindle-entry-text) :title (clean title) :location location :page-number page-number))))

(defun get-note-headers (&optional (path "./kindle-notes.txt"))
  (flet ((read-from-file (path)
	   (with-open-file (stream path)
	     (let ((kindle-entries (make-array 0 :element-type 'kindle-entry :fill-pointer 0 :adjustable t))
		   (previous-line nil))
	       (loop
		 for line = (read-line stream nil nil)
		 while line
		 do
		    (when (null previous-line)
		      (setf previous-line line))
		    (when (string= "==========" line)
		      (vector-push-extend (parse-kindle-entry stream previous-line) kindle-entries))
		    (setf previous-line line))
	       kindle-entries))))
    (read-from-file path)))

(defgeneric empty-entry-p (kindle-entry)
  (:documentation "Is this an empty entry?"))
;; (documentation 'remove-empty-entries 'function)

(defmethod empty-entry-p ((kindle-entry kindle-entry))
  "Is this an empty entry?"
  (with-slots (text) kindle-entry
    (and text (not (zerop (length text))))))
;; how to find: (find-method #'make-book-with-quantity-1 nil (list (find-class 'book)))

(defun remove-empty-entries (kindle-entries)
  "Remove any entries without any text."
  (remove-if-not #'empty-entry-p kindle-entries))

(defparameter *note-headers* (remove-empty-entries (get-note-headers)))

(defun show-tip-of-the-day (&optional (note-headers *note-headers*))
  (aref note-headers (random (length note-headers))))

(defun print-org-table (&optional (note-headers *note-headers*) (abbreviated t))
  "Print kindle entries in org table format."
  (with-output-to-string (stream)
    (format stream " | Text | Title | Location | Page No |~%")
    (loop for kindle-entry across note-headers
	  do
	     (print-org-table-row kindle-entry stream abbreviated))))

(defgeneric print-org-table-row (kindle-entry stream abbreviated)
  (:documentation "Print kindle-entry as an org table row."))
;; (documentation 'print-org-table-row 'function)

(defmethod print-org-table-row ((kindle-entry kindle-entry) stream abbreviated)
  "Print kindle-entry as an org table row."
  (with-slots (text title location page-number) kindle-entry
    (let ((formatted-text (if abbreviated (subseq text 0 (min (length text) 15)) text)))
      (format stream " | ~a | ~a | ~a | ~a |~%" formatted-text title (if location location "") (if page-number page-number "")))))
;; how to find: (find-method #'print-org-table-row nil (list (find-class 'kindle-entry)))
