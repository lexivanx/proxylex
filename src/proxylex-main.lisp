;;;; Reverse proxy and load balancer in Lisp
;;;; github.com/lexivanx/proxylex
;;;; For intro to Lisp check the docs
;;;; common-lisp.net/documentation

(ql:quickload '("usocket" "drakma" "local-time" "cl-cache" "bordeaux-threads"))

(defpackage :reverse-proxy
  (:use :cl :usocket :drakma :local-time :cl-cache :bordeaux-threads))

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

;;; Rate limiting
(defvar *request-rate* 10) ; 10 requests per minute
(defvar *client-request-count* (make-hash-table :test 'equal))

(defun check-rate-limit (client-ip)
  (let ((current-time (now))
        (count-data (gethash client-ip *client-request-count*)))
    (if count-data
        (let ((start-time (car count-data))
              (count (cdr count-data)))
          (if (> (subtract-time current-time start-time) (parse-relative-time "1min"))
              (progn
                (setf (gethash client-ip *client-request-count*) (cons current-time 1))
                t)
              (if (< count *request-rate*)
                  (progn
                    (incf (cdr count-data))
                    t)
                  nil)))
        (progn
          (setf (gethash client-ip *client-request-count*) (cons current-time 1))
          t))))

;;; Caching
(defvar *response-cache* (make-cache :test 'equal :max-size 100 :eviction-policy :lru))

(defun cache-response (request-uri status headers body)
  (let ((cache-entry (cons status (cons headers body))))
    (cache-put *response-cache* request-uri cache-entry)))

(defun get-cached-response (request-uri)
  (cache-get *response-cache* request-uri))

(defun process-request-with-caching (request-stream)
  (let* ((request-uri (read-line request-stream))
         (cached-response (get-cached-response request-uri)))
    (if cached-response
        (values (car cached-response)
                (cadr cached-response)
                (cddr cached-response))
        (multiple-value-bind (status headers body)
            (process-request request-stream)
          (cache-response request-uri status headers body)
          (values status headers body)))))

;;; Like handle-client with added caching
(defun handle-client-with-caching (client-stream)
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

;;; Init reverse proxy on port 8082
(defun main ()
  (let ((port 8082))
    (format t "Reverse proxy started on port ~A.~%" port)
    ;; Or call start-proxy-with-caching
    ;; for caching functionality 
    (start-proxy port)))

;;; Call main
(main)
