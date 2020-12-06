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

(defpackage :day6
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day6)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input6" using #'read-line)
        (collect line)))

(defun read-group (lines)
  (iter (with table = (make-hash-table))
        (for rem first lines then (cdr rem))
        (while (and rem (> (length (car rem)) 0)))
        (iter (for c in-string (car rem))
              (setf (gethash c table) t))
        (finally (return (list table (cdr rem))))))

(defun count-all-yes (lines)
  (iter (for rem first lines then (cdr rem))
        (while (and rem (> (length (car rem)) 0)))
        (reducing (map 'list #'identity (car rem)) by #'intersection into res)
        (finally (return (list (length res) (cdr rem))))))

(defun part-1 ()
  (iter (with lines = (read-lines))
        (for (group rem) first (read-group lines) 
             then (read-group rem))
        (summing (hash-table-count group))
        (while (cdr rem))))

(defun part-2 ()
  (iter (with lines = (read-lines))
        (for (count rem) first (count-all-yes lines)
             then (count-all-yes rem))
        (summing count)
        (while (cdr rem))))
