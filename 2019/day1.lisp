(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :ironclad)
(ql:quickload :flexi-streams)

(defpackage :day1
  (:use :cl :cl-ppcre :iterate :ironclad :flexi-streams))

(in-package :day1)

(defun find-hash (num)
  (let* ((input (format nil "~a" num)))
    (digest-sequence :md5 (string-to-octets input))))

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "\\d+" line)))

(defun read-syms (line)
  (let (*read-eval*)
    (with-input-from-string (s line)
      (iter (for sym in-stream s)
            (collect sym)))))

(defun answer-1 ()
  (iter (for line in-file "input1" using #'read-line)
        (for m = (parse-integer line))
        (sum (- (floor m 3) 2))))

(defun total-fuel (m)
  (let ((current (- (floor m 3) 2)))
    (if (<= current 0)
        0
        (+ current (total-fuel current)))))

(defun answer-2 ()
  (iter (for line in-file "input1" using #'read-line)
        (for m = (parse-integer line))
        (sum (total-fuel m))))
