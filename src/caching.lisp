(ql:quickload '("cl-cache"))

(defpackage :reverse-proxy-caching
  (:use :cl :cl-cache :drakma))

(in-package :reverse-proxy-caching)

(defvar *response-cache* (make-cache :test 'equal :max-size 100 :eviction-policy :lru))

(defun cache-response (request-uri status headers body)
  (let ((cache-entry (cons status (cons headers body))))
    (cache-put *response-cache* request-uri cache-entry)))

(defun get-cached-response (request-uri)
  (cache-get *response-cache* request-uri))

(defun process-request-with-caching (request-stream backend-servers)
  (let* ((request-uri (read-line request-stream))
         (cached-response (get-cached-response request-uri)))
    (if cached-response
        (values (car cached-response)
                (cadr cached-response)
                (cddr cached-response))
        (multiple-value-bind (status headers body)
            (process-request request-stream backend-servers)
          (cache-response request-uri status headers body)
          (values status headers body)))))
