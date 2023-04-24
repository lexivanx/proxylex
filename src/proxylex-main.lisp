;;;; Reverse proxy and load balancer in Lisp
;;;; github.com/lexivanx/proxylex
;;;; For intro to Lisp check the docs
;;;; common-lisp.net/documentation

(ql:quickload '("usocket" "drakma" "local-time" "cl-cache" "bordeaux-threads" "cl-json" "cl+ssl"))

(defpackage :reverse-proxy
  (:use :cl :usocket :drakma :local-time :cl-cache :bordeaux-threads :cl-json :cl+ssl))

(in-package :reverse-proxy)

(defun read-config (filename)
  (with-open-file (in filename)
    (json:decode-json in)))

;;; Read configuration from the JSON file and store the values in global variables
(defvar *config* (read-config "config.json"))
(defvar *backend-servers* (gethash "backend_servers" *config*))
(defvar *proxy-port* (gethash "proxy_port" *config*))
(defvar *request-rate* (gethash "request_rate" *config*))
(defvar *certificate-path* (gethash "certificate_path" *config*))
(defvar *private-key-path* (gethash "private_key_path" *config*))

;;; Select server randomly from backend-servers variable
(defun choose-backend ()
  (let ((index (random (length *backend-servers*))))
    (nth index *backend-servers*)))

;;; Process request via Drakma HTTP client
(defun process-request (request-stream &optional (retries 3))
  (loop repeat retries
        for attempt from 1
        do (handler-case
               (let* ((request (drakma:http-request (concatenate 'string (choose-backend) (read-line request-stream)) :stream request-stream))
                      (status (drakma:http-response-status request))
                      (headers (drakma:http-response-headers request))
                      (body (drakma:http-response-body request)))
                 (return (values status headers body)))
             (drakma:http-condition (e)
               (if (= attempt retries)
                   (return (values 503 nil "Error: backend server unavailable."))
                   (format t "Error: ~A~% Retrying...~%" e))))))

;;; Helper function to upgrade client connection to SSL
(defun upgrade-client-stream-to-ssl (client-stream)
  (let* ((ssl-context (ssl:make-ssl-client-context))
         (ssl-stream (ssl:make-ssl-stream client-stream ssl-context)))
    (ssl:load-certificate-chain-from-file ssl-context *certificate-path*)
    (ssl:load-private-key-from-file ssl-context *private-key-path*)
    (setf (usocket:stream-type client-stream) ssl-stream)))

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

;;; Rate limiting
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

;;; Init reverse proxy on port 8082
(defun main ()
  (format t "Reverse proxy started on port ~A.~%" *proxy-port*)
  ;; Or call start-proxy-with-caching
  ;; for caching functionality 
  (start-proxy *proxy-port*))

;;; Call main
(main)
