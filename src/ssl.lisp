(ql:quickload '("cl+ssl"))

(defpackage :reverse-proxy-ssl
  (:use :cl :cl+ssl :usocket))

(in-package :reverse-proxy-ssl)

(defun upgrade-client-stream-to-ssl (client-stream certificate-path private-key-path)
  (let* ((ssl-context (ssl:make-ssl-client-context))
         (ssl-stream (ssl:make-ssl-stream client-stream ssl-context)))
    (ssl:load-certificate-chain-from-file ssl-context certificate-path)
    (ssl:load-private-key-from-file ssl-context private-key-path)
    (setf (usocket:stream-type client-stream) ssl-stream)))
