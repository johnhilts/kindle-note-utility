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

(defun get-note-headers (&optional (path "./kindle-notes.txt"))
  (flet ((get-location-info-start-position (line-with-location-info)
	   (let ((marker-1-position (search *location-marker-1* line-with-location-info))
		 (marker-2-position (search *location-marker-2* line-with-location-info)))
	     (when (or
		    marker-1-position
		    marker-2-position)
	       (+
		(if marker-1-position marker-1-position marker-2-position)
		(if marker-1-position (length *location-marker-1*) (length *location-marker-2*))))))
	 (get-page (line-with-location-info)
	   (let ((marker-start (search *page-marker* line-with-location-info))
		 (page-info-start (position-if (lambda (c) (digit-char-p c)) line-with-location-info)))
	     (if (and
		  marker-start
		  (< page-info-start marker-start))
		 (parse-integer
		  (subseq line-with-location-info
			  page-info-start
			  (+
			   page-info-start
			   (position-if-not (lambda (c) (digit-char-p c)) line-with-location-info :start page-info-start)))
		  :junk-allowed t)
		 nil)))
	 (clean (string)
	   (delete #\Return string)))
    (with-open-file (s path)
      (let ((kindle-entries ())
	    (previous-line nil))
	(loop
	  for line = (read-line s nil nil)
	  while line
	  do
	     (when (null previous-line)
	       (setf previous-line line))
	     (when (string= "==========" line)
	       (let* ((title (read-line s nil nil))
		      (line-with-location-info (read-line s nil nil))
		      (location-info-start-position (get-location-info-start-position line-with-location-info))
		      (location-info (if location-info-start-position
					 (subseq
					  line-with-location-info
					  location-info-start-position
					  (+ location-info-start-position (position-if-not #'digit-char-p (subseq line-with-location-info location-info-start-position))))
					 nil))
		      (page-number (get-page line-with-location-info)))
		 (push (make-instance 'kindle-entry :text (clean previous-line) :title (clean title) :location location-info :page-number page-number) kindle-entries)))
	     (setf previous-line line))
	(nreverse kindle-entries)))))

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
  (nth (random (length note-headers)) note-headers))
