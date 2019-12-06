(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :alexandria)

(defpackage :day22 
  (:use :cl :cl-ppcre :iterate :anaphora :alexandria))

(in-package :day22)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-nodes ()
  (map 'vector #'identity
       (iter (for line in-file "input22" using #'read-line)
             (awhen (ints line)
                 (collect it)))))

(defun viable-pairs ()
  (let ((nodes (read-nodes)))
    (iter outer (for i from 0 to (- (length nodes) 2))
          (for (x1 y1 size1 used1 avail1 use1) = (aref nodes i))
          (iter (for j from (+ i 1) to (- (length nodes) 1))
                (for (x2 y2 size2 used2 avail2 use2) = (aref nodes j))
                (in outer (count (or (and (> used1 0) (<= used1 avail2))
                                     (and (> used2 0) (<= used2 avail1)))))))))

