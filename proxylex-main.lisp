;;;; Reverse proxy and load balancer in Lisp
;;;; github.com/lexivanx/proxylex
;;;; For intro to Lisp check the docs
;;;; common-lisp.net/documentation

(ql:quickload '("usocket" "drakma"))

(defpackage :reverse-proxy
  (:use :cl :usocket :drakma))

(in-package :reverse-proxy)

;;; List of backend servers
(defvar *backend-servers* '("http://localhost:8080" "http://localhost:8081"))

;;; Select server randomly from backend-servers variable
(defun choose-backend ()
  (let ((index (random (length *backend-servers*))))
    (nth index *backend-servers*)))

;;; Process request via Drakma HTTP client
(defun process-request (request-stream)
  (let* ((request (drakma:http-request (concatenate 'string (choose-backend) (read-line request-stream)) :stream request-stream))
         (status (drakma:http-response-status request))
         (headers (drakma:http-response-headers request))
         (body (drakma:http-response-body request)))
    (values status headers body)))

;;; Handle client connection and send response to client
(defun handle-client (client-stream)
  (let ((status nil)
        (headers nil)
        (body nil))
    (multiple-value-setq (status headers body) (process-request client-stream))
    (format client-stream "HTTP/1.1 ~A~C~C" status #\return #\newline)
    (loop for (name . value) in headers
          do (format client-stream "~A: ~A~C~C" name value #\return #\newline))
    (format client-stream "~C~C" #\return #\newline)
    (write-sequence body client-stream)
    (finish-output client-stream)
    (close client-stream)))

;;; Start server socket and accept client connections in loop
(defun start-proxy (port)
  (let ((server-socket (socket-listen usocket:*wildcard-host* port :reuse-address t)))
    (unwind-protect
         (loop (let ((client-socket (socket-accept server-socket)))
                 (handler-case
                     (let ((client-stream (socket-stream client-socket)))
                       (handle-client client-stream))
                   (error (c) (format t "Error: ~A~%" c)))))
      (socket-close server-socket))))

;;; Init reverse proxy on port 8082
(defun main ()
  (let ((port 8082))
    (format t "Reverse proxy started on port ~A.~%" port)
    (start-proxy port)))

;;; Init
(main)
