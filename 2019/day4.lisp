(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :ironclad)
(ql:quickload :flexi-streams)
(ql:quickload :anaphora)

(defpackage :day4
  (:use :cl :cl-ppcre :iterate :ironclad :flexi-streams :anaphora))

(in-package :day4)

(defun adjacent-same (i)
  (scan "(\\d)\\1" (format nil "~a" i)))

(defun increasing-digits (i)
  (iter (for j first i then (floor j 10))
        (while (> j 0))
        (for k = (mod j 10))
        (for k-p previous k)
        (when (and k-p (> k k-p))
          (return-from increasing-digits nil)))
  t)

(defun non-dup-same (i)
  (iter (for j first i then (floor j 10))
        (while (> j 0))
        (for k = (mod j 10))
        (for k-p previous k)
        (for k-pp previous k-p)
        (for k-ppp previous k-pp)
        (when (or (and k-pp k-p (not k-ppp)
                       (and (= k-pp k-p)
                            (/= k-p k)))
                  (and k-ppp k-pp k-p 
                       (= k-p k-pp)
                            (/= k k-p)
                            (/= k-pp k-ppp))
                  (and (= (floor j 10) 0)
                       k-p k-pp 
                       (= k k-p)
                       (/= k-p k-pp)))
          (return-from non-dup-same t))))

(defun answer-1 ()
  (iter (for i from 265275 to 781584)
        (count (and (increasing-digits i)
                   (adjacent-same i)))))

(defun answer-2 ()
  (iter (for i from 265275 to 781584)
        (when (and (increasing-digits i)
                    (non-dup-same i)))
        (count (and (increasing-digits i)
                    (non-dup-same i)))))
