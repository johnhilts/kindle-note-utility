(in-package #:cl-user)

(defpackage #:jfh-kindle-notes
  (:use #:common-lisp)
  (:export
   #:kindle-entry
   #:refresh-note-headers
   #:show-tip-of-the-day
   #:format-object
   #:print-object))

(defpackage #:jfh-kindle-notes-util
  (:use #:common-lisp)
  (:export
   #:list-titles
   #:search-for))
