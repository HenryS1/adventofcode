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

(defpackage :day15
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day15)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input15" using #'read-line)
        (collect line)))

(defparameter *nums* (list 9 19 1 6 0 5 4))

(defun spoken-time (turn)
  (let ((spoken (make-hash-table)))
    (iter (for n in (butlast *nums*))
          (for i from 1)
          (setf (gethash n spoken) i))
    (iter (for i from (length *nums*))
          (for n first (car (last *nums*)) 
               then (if (gethash n spoken) (- i (gethash n spoken) 1) 0))
          (for np previous n)
          (setf (gethash np spoken) (- i 1))
          (when (= i turn)
            (return n)))))

(defun part-1 () (spoken-time 2020))
(defun part-2 () (spoken-time 30000000))
