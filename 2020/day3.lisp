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

(defpackage :day3
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day3)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input3" using #'read-line)
        (collect line result-type 'vector)))

(defun collisions (lines right down)
  (iter (with len = (length (aref lines 0)))
        (for row first 0 then (+ row down))
        (while (< row (length lines)))
        (for line = (aref lines row))
        (for i first 0 then (mod (+ i right) len))
        (count (char= (aref line i) #\#))))

(defun part-1 ()
  (collisions (read-lines) 3 1))

(defun part-2 ()
  (let ((lines (read-lines)))
   (reduce #'* (mapcar (lambda (right down) (collisions lines right down))
                       '(1 3  5 7 1) '(1 1 1 1 2)))))
