(in-package #:jfh-kindle-notes-util)

(defun list-titles (&optional search)
  "Output a list of titles."
  (format nil "~{~A~^~%~}"
          (sort
           (copy-list
            (remove-duplicates
             (map 'list
                  (lambda (note) (jfh-kindle-notes::title note))
                  (if search
                      (remove-if-not (lambda (note) (search search (jfh-kindle-notes::title note) :test #'string-equal)) jfh-kindle-notes::*note-headers*)
                      jfh-kindle-notes::*note-headers*))
             :test #'string-equal))
           #'string<)))

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
