(cl:in-package :thread-safety-test)

(defvar *start-event* (sb-thread:make-waitqueue))
(defvar *started-event* (sb-thread:make-waitqueue))
(defvar *the-lock* (sb-thread:make-mutex :name "the lock"))
(defvar *started-count* 0)
(defvar *num-threads* 2)
(defparameter *the-timeout* 5)

(defun thread-task (id)
  (sb-thread:with-mutex (*the-lock*)
    (incf *started-count*)
    (when (>= *started-count* *num-threads*)
      (bt:condition-notify *started-event*))
    (bt:condition-wait *start-event* *the-lock*))
  ;; Replace the following line with your critical section
  (format t "Thread ~A is running at ~A~%" id (get-universal-time)))

(defun run-test ()
  (let ((threads (loop for i from 1 to *num-threads* collect (bt:make-thread (lambda () (thread-task i))))))
    (sb-thread:with-mutex (*the-lock*)
      (loop until (>= *started-count* *num-threads*)
            do (bt:condition-wait *started-event* *the-lock*)))
    (sleep *the-timeout*)
    (sb-thread:with-mutex (*the-lock*)
      (bt:condition-notify *start-event*))
    (mapc #'bt:join-thread threads)))