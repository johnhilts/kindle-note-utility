(cl:in-package :thread-safety-test)

(defvar *start-event* (sb-thread:make-waitqueue))
(defvar *the-lock* (sb-thread:make-mutex :name "the lock"))

(defun thread-task (id)
  (bt:with-timeout (10)
    (bt:condition-wait *start-event* *the-lock*))
  ;; Replace the following line with your critical section
  (format t "Thread ~A is running at ~A~%" id (get-universal-time)))

;; (defun thread-task (id)
;;   (bt:with-timeout (10 (format t "Thread ~A timed out~%" id))
;;     (condition-wait *start-event*))
;;   ;; Replace the following line with your critical section
;;   (format t "Thread ~A is running at ~A~%" id (get-universal-time)))

(defun run-test ()
  (let ((threads (loop for i from 1 to 2 collect (bt:make-thread (lambda () (thread-task i))))))
    (sleep 5) ; Wait for 10 seconds
    (bt:condition-notify *start-event*)
    (mapc #'bt:join-thread threads)))
