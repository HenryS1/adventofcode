(defpackage :aoc.functional
  (:use :cl :metabang-bind)
  (:export :take :take-drop-while :take-drop :take-seq))

(in-package :aoc.functional)

(defun take-seq (n l)
  (subseq l 0 n))

(defun take-drop-while (ls p)
  (labels ((rec (acc rest)
             (cond ((null rest) (cons (reverse acc) rest))
                   ((funcall p (car rest))
                    (rec (cons (car rest) acc) (cdr rest)))
                   (t (cons (reverse acc) rest)))))
    (rec nil ls)))

(defun take-drop (n l)
  (if (or (= n 0) (null l)) 
      (cons nil l)
      (bind (((hd . tl) (take-drop (- n 1) (cdr l))))
        (cons (cons (car l) hd) tl))))

(defun take (n l)
  (if (or (null l) (= n 0))
      nil
      (cons (car l) (take (- n 1) (cdr l)))))


