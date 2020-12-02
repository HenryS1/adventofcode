(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :trivia)
  (ql:quickload :trivia.ppcre)
  (ql:quickload :cl-ppcre)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind)
  (ql:quickload :alexandria)
  (ql:quickload :cl-arrows)
  (load "../2018/queue.lisp")
  (load "../2018/priority-queue.lisp")) 

(defpackage :day2
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre :iterate :alexandria :anaphora :metabang-bind))

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
  (match line 
    ((ppcre "(\\d+)-(\\d+) (\\w): (\\w+)" (read mn) (read mx) c str)
     (<= mn (count-if (lambda (ch) (char= (aref c 0) ch)) str) mx))))

(defun part-1 () (count-if #'check-rule (read-lines)))

(defun check-index-rule (line)
  (match line
    ((ppcre "(\\d+)-(\\d+) (\\w): (\\w+)" (read mn) (read mx) c str)
     (let ((one (char= (aref str (- mn 1)) (aref c 0)))
           (other (char= (aref str (- mx 1)) (aref c 0))))
       (or (and one (not other))
           (and other (not one)))))))

(defun part-2 () (count-if #'check-index-rule (read-lines)))
