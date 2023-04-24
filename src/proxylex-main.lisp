;;;; Reverse proxy and load balancer in Lisp
;;;; github.com/lexivanx/proxylex
;;;; Check out the CL docs:
;;;; common-lisp.net/documentation

(ql:quickload '("usocket" "drakma" "local-time" "cl-cache" "bordeaux-threads" "cl-json" "cl+ssl"))

(defpackage :reverse-proxy
  (:use :cl :usocket :bordeaux-threads))

(in-package :reverse-proxy)

(load "config.lisp")
(load "backend.lisp")
(load "ssl.lisp")
(load "rate-limiting.lisp")
(load "caching.lisp")

(use-package :reverse-proxy-config)
(use-package :reverse-proxy-backend)
(use-package :reverse-proxy-ssl)
(use-package :reverse-proxy-rate-limiting)
(use-package :reverse-proxy-caching)

;;; Read configuration from the JSON file and store the values in global variables
(defvar *config* (read-config "config.json"))
(defvar *backend-servers* (gethash "backend_servers" *config*))
(defvar *proxy-port* (gethash "proxy_port" *config*))
(defvar *request-rate* (gethash "request_rate" *config*))
(defvar *certificate-path* (gethash "certificate_path" *config*))
(defvar *private-key-path* (gethash "private_key_path" *config*))

;;; Handle client connection and send response to client
(defun handle-client (client-stream)
  (upgrade-client-stream-to-ssl client-stream)
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

;;; Like handle-client with added caching
(defun handle-client-with-caching (client-stream)
  (upgrade-client-stream-to-ssl client-stream)
  (let ((status nil)
        (headers nil)
        (body nil))
    (multiple-value-setq (status headers body) (process-request-with-caching client-stream))
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
                 (bt:make-thread (lambda ()
                                   (handler-case
                                       (let ((client-stream (socket-stream client-socket)))
                                         (handle-client client-stream))
                                     (error (c) (format t "Error: ~A~%" c)))))))
      (socket-close server-socket))))

;;; Identical to start-proxy, with caching
(defun start-proxy-with-caching (port)
  (let ((server-socket (socket-listen usocket:*wildcard-host* port :reuse-address t)))
    (unwind-protect
         (loop (let ((client-socket (socket-accept server-socket)))
                 (bt:make-thread (lambda ()
                                   (handler-case
                                       (let ((client-stream (socket-stream client-socket)))
                                         (handle-client-with-caching client-stream))
                                     (error (c) (format t "Error: ~A~%" c)))))))
      (socket-close server-socket))))

;;; Init reverse proxy
(defun main ()
  (format t "Reverse proxy started on port ~A.~%" *proxy-port*)
  ;; Or call start-proxy-with-caching
  ;; for caching functionality 
  (start-proxy *proxy-port*))

;;; Call main
(main)
