(ql:quickload '("local-time"))

(defpackage :reverse-proxy-rate-limiting
  (:use :cl :local-time))

(in-package :reverse-proxy-rate-limiting)

(defvar *client-request-count* (make-hash-table :test 'equal))

(defun check-rate-limit (client-ip request-rate)
  (let ((current-time (now))
        (count-data (gethash client-ip *client-request-count*)))
    (if count-data
        (let ((start-time (car count-data))
              (count (cdr count-data)))
          (if (> (subtract-time current-time start-time) (parse-relative-time "1min"))
              (progn
                (setf (gethash client-ip *client-request-count*) (cons current-time 1))
                t)
              (if (< count request-rate)
                  (progn
                    (incf (cdr count-data))
                    t)
                  nil)))
        (progn
          (setf (gethash client-ip *client-request-count*) (cons current-time 1))
          t))))
