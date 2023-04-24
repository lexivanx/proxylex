(ql:quickload '("cl-json"))

(defpackage :reverse-proxy-config
  (:use :cl :cl-json))

(in-package :reverse-proxy-config)

(defun read-config (filename)
  (with-open-file (in filename)
    (json:decode-json in)))
