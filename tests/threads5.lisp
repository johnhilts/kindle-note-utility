(cl:in-package :thread-safety-test)

(defparameter *start-event* (sb-thread:make-waitqueue))
(defparameter *started-event* (sb-thread:make-waitqueue))
(defparameter *the-lock* (sb-thread:make-mutex :name "the lock"))
(defparameter *queue-lock* (sb-thread:make-mutex :name "queue lock"))
(defparameter *started-count* 0)
(defparameter *num-threads* 5)
(defparameter *the-timeout* 3)

;; (defun thread-task (id)
;;   (sb-thread:with-mutex (*the-lock*)
;;     (incf *started-count*)
;;     (when (>= *started-count* *num-threads*)
;;       (bt:condition-notify *started-event*))
;;     (bt:condition-wait *start-event* *the-lock*))
;;   ;; Replace the following line with your critical section
;;   (format t "Thread ~A is running at ~A~%" id (get-universal-time))
;;   (add-to-queue (format nil "Thread ~D" id))
;;   (values))

(defun thread-task (id)
  (sb-thread:with-mutex (*the-lock*)
    (bt:condition-wait *start-event* *the-lock*))
  ;; Add to queue
  (add-to-queue (format nil "Thread ~D data" id)))

(defun get-threads ()
  (loop for i from 1 to *num-threads*
        collect
        (let ((id i))
          (bt:make-thread (lambda () (thread-task id)) :name (format nil "Thread #~D" id)))))

(defun run-test ()
  (let ((threads (get-threads)))
    (sb-thread:with-mutex (*the-lock*)
      (loop until (>= *started-count* *num-threads*)
            do (bt:condition-wait *started-event* *the-lock*)))
    (sleep *the-timeout*)
    (sb-thread:with-mutex (*the-lock*)
      (sb-thread:condition-broadcast *start-event*))
    (mapc #'bt:join-thread threads)))

(defparameter *my/queue* '())

(defun add-to-queue (item)
  "enqueue"
  (sb-thread:with-mutex (*queue-lock*)
    (push item *my/queue*)))
