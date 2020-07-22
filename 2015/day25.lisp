(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :metabang-bind)
  (ql:quickload :iterate))

(defpackage :day25
  (:use :cl :cl-ppcre :bind :iterate))

(in-package :day25)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun find-code-index (row col)
  (let* ((mx-row (+ row (- col 1)))
         (triangle (floor (* mx-row (- mx-row 1)) 2)))
    (+ triangle col)))

(defun pow-mod (base pow md)
  (labels ((rec (curr pw)
             (cond ((= pw 0) 1)
                   ((= (mod pw 2) 0)
                    (let ((rm (rec curr (floor pw 2))))
                      (mod (* rm rm) md)))
                   (t (let ((rm (rec curr (floor pw 2))))
                        (mod (* base rm rm) md))))))
    (rec base pow)))

(defun part-one ()
  (bind (((row col) (ints (with-open-file (f "input25") (read-line f))))
         (pw (find-code-index row col))
         (md 33554393)
         (pm (pow-mod 252533 (- pw 1) md)))
    (mod (* 20151125 pm) md)))
