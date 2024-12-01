(defpackage :day1
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind)
  (:export 
   :calibration-value
   :total-calibration-value
   :digits-in-line
   :all-calibration-values))

(in-package :day1)

(defun ints (line) 
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" line)))

(defun read-input (file)
  (with-open-file (f file)
    (loop for line = (read-line f nil nil)
          while line collect (ints line))))

(defun separate-pairs (pairs)
  (loop for pair in pairs
        collect (car pair) into left
        collect (cadr pair) into right
        finally (return (cons left right))))

(defun pair-difference (pairs)
  (let* ((separated (separate-pairs pairs))
         (left (sort (car separated) #'<))
         (right (sort (cdr separated) #'<)))
    (loop for one in left 
          for other in right
          summing (abs (- one other)))))

(defun part1 ()
  (let ((pairs (read-input "day1input")))
    (pair-difference pairs)))

(defun similarity-score (pairs)
  (let ((separated (separate-pairs pairs)))
    (loop with (left . right) = separated 
          for one in left
          summing (* one (count one right)))))

(defun part2 ()
  (let ((pairs (read-input "day1input")))
    (similarity-score pairs)))
