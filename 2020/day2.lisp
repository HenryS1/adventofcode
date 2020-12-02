(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-ppcre)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind)
  (ql:quickload :alexandria)
  (ql:quickload :cl-arrows)
  (load "../2018/queue.lisp")
  (load "../2018/priority-queue.lisp")) 

(defpackage :day2
  (:use :cl :cl-ppcre :cl-arrows :iterate :alexandria :anaphora :metabang-bind))

(in-package :day2)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input2" using #'read-line)
        (collect line)))

(defun check-rule (line)
  (bind (((:values _ registers) (scan-to-strings "(\\d+)-(\\d+) (\\w): (\\w+)" line))
         (mn (parse-integer (aref registers 0)))
         (mx (parse-integer (aref registers 1)))
         (c (aref (aref registers 2) 0))
         (str (aref registers 3)))
    (<= mn (count-if (lambda (ch) (char= c ch)) str) mx)))

(defun check-index-rule (line)
  (bind (((:values _ registers) (scan-to-strings "(\\d+)-(\\d+) (\\w): (\\w+)" line))
         (mn (parse-integer (aref registers 0)))
         (mx (parse-integer (aref registers 1)))
         (c (aref (aref registers 2) 0))
         (str (aref registers 3)))
    (or (and (char= (aref str (- mn 1)) c) (char/= (aref str (- mx 1)) c))
        (and (char= (aref str (- mx 1)) c) (char/= (aref str (- mn 1)) c)))))

(defun part-2 ()
  (count-if #'check-index-rule (read-lines)))
