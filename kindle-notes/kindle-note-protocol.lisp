(in-package #:jfh-kindle-notes)

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

(defgeneric empty-entry-p (kindle-entry)
  (:documentation "Is this an empty entry?"))
;; (documentation 'remove-empty-entries 'function)

(defgeneric print-org-table-row (kindle-entry stream abbreviated)
  (:documentation "Print kindle-entry as an org table row."))
;; (documentation 'print-org-table-row 'function)

(defgeneric format-object (kindle-entry)
  (:documentation "Format a kindle entry."))
;; (documentation 'format-object 'function)

