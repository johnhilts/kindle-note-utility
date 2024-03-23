(cl:in-package :thread-safety-test)

(defparameter *thread-count* 2)
(defparameter *the-timeout* 5)

(defvar *start-event* (sb-thread:make-waitqueue))
(defvar *the-lock* (sb-thread:make-mutex :name "the lock"))

(defun thread-task (id)
  (sb-thread:with-mutex (*the-lock*)
    (bt:condition-wait *start-event* *the-lock*))
  ;; Replace the following line with your critical section
  (format t "Thread ~A is running at ~A~%" id (get-universal-time)))

(defun run-test ()
  (let ((threads (loop for i from 1 to *thread-count* collect (bt:make-thread (lambda () (thread-task i))))))
    (sleep *the-timeout*)
    (sb-thread:with-mutex (*the-lock*)
      (bt:condition-notify *start-event*))
    (mapc #'bt:join-thread threads)))
