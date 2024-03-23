(cl:in-package :thread-safety-test)

(defparameter *start-event* (bt:make-condition-variable :name "start-event"))
(defparameter *the-lock* (bt:make-lock))
(defparameter *queue-lock* (bt:make-lock))
(defparameter *queue* '())

(defun add-to-queue (item)
  (push item *queue*))

(defun add-to-queue-with-critical-section (item)
  (bt:with-lock-held (*queue-lock*)
    (push item *queue*)))

(defun thread-task (id)
  (bt:with-lock-held (*the-lock*)
    (bt:condition-wait *start-event* *the-lock*))
  ;; Add to queue
  (add-to-queue-with-critical-section (format nil "Thread ~D data" id)))

(defun run-test ()
  (let ((threads (loop for i from 1 to 100 collect (let ((id i)) (bt:make-thread (lambda () (thread-task id)) :name (format nil "Thread #~D" id))))))
    (sleep 5)
    (bt:with-lock-held (*the-lock*)
      (sb-thread:condition-broadcast *start-event*))
    (mapc #'bt:join-thread threads)))
