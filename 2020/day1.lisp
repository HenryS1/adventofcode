(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-ppcre)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind)
  (ql:quickload :alexandria)
  (ql:quickload :cl-arrows)
  (load "../2018/queue.lisp")
  (load "../2018/priority-queue.lisp")) 

(defpackage :day1
  (:use :cl :cl-ppcre :cl-arrows :iterate :alexandria :anaphora :metabang-bind))

(in-package :day1)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input1" using #'read-line)
        (collect (parse-integer line))))

(defun part-1 ()
  (iter (with table = (make-hash-table))
        (for line in-file "input1" using #'read-line)
        (for i = (parse-integer line))
        (setf (gethash i table) t)
        (when (gethash (- 2020 i) table)
          (return (* i (- 2020 i))))))

(defun part-2 ()
  (let ((ns (coerce (read-lines) 'vector))
        (sums (make-hash-table)))
    (iter (for i from 0 to (- (length ns) 1))
          (iter (for j from (+ i 1) to (- (length ns) 1))
                (for sm = (+ (aref ns i) (aref ns j)))
                (when (<= sm 2020)
                  (push (cons i j) (gethash sm sums)))))
    (iter outer
          (for i from 0 to (- (length ns) 1))
          (for v = (aref ns i))
          (when (gethash (- 2020 v) sums)
            (iter (for (j . k) in (gethash (- 2020 v) sums))
                  (when (and (/= i j) (/= i k))
                    (in outer (leave (* v (aref ns j) (aref ns k))))))))))
