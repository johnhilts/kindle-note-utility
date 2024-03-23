(in-package #:cl-user)

(defpackage #:jfh-internal-thread-testing
  (:use #:common-lisp)
  (:local-nicknames (#:web #:jfh-web-core) (#:auth #:jfh-web-auth) (#:web-app #:jfh-kindle-notes-web-app) (#:main #:jfh-kindle-notes-main))
  (:export
   #:%list-all-external-symbols
   #:%current-swank-port
   #:%web-app-running-p
   #:%clear-static-routes))

(in-package #:jfh-internal-thread-testing)

;;; simple queue example
(ql:quickload '("bordeaux-threads" "cl-containers"))

(defpackage :queue-example
  (:use :cl :bordeaux-threads :cl-containers))

(in-package :queue-example)

(defvar *task-queue* (make-instance 'queue-container))

(defun enqueue-task (task)
  (enqueue *task-queue* task))

(defun dequeue-task ()
  (dequeue *task-queue*))

(defun process-tasks ()
  (let ((task (dequeue-task)))
    (when task
      (funcall task)
      (process-tasks))))

(defun start-processing-tasks ()
  (make-thread #'process-tasks))

;;; simple queue example usage
(enqueue-task (lambda () (format t "Task 1~%")))
(enqueue-task (lambda () (format t "Task 2~%")))
(start-processing-tasks)

;;; busy-waiting
(ql:quickload '("bordeaux-threads"))

(defpackage :thread-safety-test
  (:use :cl :bordeaux-threads))

(in-package :thread-safety-test)

(defvar *start-time* (get-universal-time))

(defun thread-task (id)
  (loop until (>= (get-universal-time) *start-time*))
                                        ; Replace the following line with your critical section
  (format t "Thread ~A is running at ~A~%" id (get-universal-time)))

(defun run-test ()
  (setf *start-time* (+ (get-universal-time) 10))
  (let ((threads (loop for i from 1 to 100 collect (make-thread (lambda () (thread-task i))))))
    (mapc #'join-thread threads)))

;;; waiting with synchronization primitives
(ql:quickload '("bordeaux-threads"))

(defpackage :thread-safety-test
  (:use :cl :bordeaux-threads))

(in-package :thread-safety-test)

(defvar *start-event* (make-waitqueue))

(defun thread-task (id)
  (with-timeout (10 (format t "Thread ~A timed out~%" id))
    (condition-wait *start-event*))
                                        ; Replace the following line with your critical section
  (format t "Thread ~A is running at ~A~%" id (get-universal-time)))

(defun run-test ()
  (let ((threads (loop for i from 1 to 100 collect (make-thread (lambda () (thread-task i))))))
    (sleep 10) ; Wait for 10 seconds
    (condition-notify *start-event*)
    (mapc #'join-thread threads)))
