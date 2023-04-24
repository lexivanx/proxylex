(ql:quickload '("drakma"))

(defpackage :reverse-proxy-backend
  (:use :cl :drakma))

(in-package :reverse-proxy-backend)

(defun choose-backend (backend-servers)
  (let ((index (random (length backend-servers))))
    (nth index backend-servers)))

(defun process-request (request-stream backend-servers &optional (retries 3))
  (loop repeat retries
        for attempt from 1
        do (handler-case
               (let* ((request (drakma:http-request (concatenate 'string (choose-backend backend-servers) (read-line request-stream)) :stream request-stream))
                      (status (drakma:http-response-status request))
                      (headers (drakma:http-response-headers request))
                      (body (drakma:http-response-body request)))
                 (return (values status headers body)))
             (drakma:http-condition (e)
               (if (= attempt retries)
                   (return (values 503 nil "Error: backend server unavailable."))
                   (format t "Error: ~A~% Retrying...~%" e))))))
