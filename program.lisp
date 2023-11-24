(in-package #:cl-user)
(defpackage #:jfh/kindle-notes
  (:use #:common-lisp))

(in-package #:jfh/kindle-notes)

(defun get-note-headers (&optional (path "c:/code/lisp/kindle/"))
  (flet ((get-location-info-start-position (line-with-location-info)
	   (let* ((marker-1 "位置No. ")
		  (marker-2 "位置 No. ")
		  (marker-1-position (search marker-1 line-with-location-info))
		  (marker-2-position (search marker-2 line-with-location-info)))
	     (when (or
		    marker-1-position
		    marker-2-position)
	       (+
		(if marker-1-position marker-1-position marker-2-position)
		(if marker-1-position (length marker-1) (length marker-2))))))
	 (get-page (line-with-location-info)
	   (let* ((marker "ページ")
		  (marker-start (search marker line-with-location-info))
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
		 -1))))
    (with-open-file (s (concatenate 'string path "My Clippings.txt"))
      (let ((lines ())
	    (previous-line nil))
	(loop
	  for line = (read-line s nil nil)
	  while line
	  do
	     (when (null previous-line)
	       (setf previous-line line))
	     ;; (break)
	     (print line)
	     (when (string= "==========" line)
	       (let* ((title (read-line s nil nil))
		      (line-with-location-info (read-line s nil nil))
		      (location-info-start-position (get-location-info-start-position line-with-location-info))
		      (location-info (if location-info-start-position
					 (concatenate
					  'string "Location: "
					  (subseq
					   line-with-location-info
					   location-info-start-position
					   (+ location-info-start-position (position-if-not #'digit-char-p (subseq line-with-location-info location-info-start-position)))))
					 (format nil "Page: ~d" (get-page line-with-location-info)))))
		 (push (list previous-line title location-info) lines)))
	     (setf previous-line line))
	lines))))

(defparameter *note-headers* (remove-if-not (lambda (line) (and line (not (zerop (length line))))) (mapcar (lambda (line) (delete #\Return line :test #'string=)) (get-note-headers))))

(defun show-tip-of-the-day (&optional (note-headers *note-headers*))
  (nth (random (length note-headers)) note-headers))
