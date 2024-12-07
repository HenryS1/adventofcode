(defpackage :day7
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :metabang-bind)
  (:export 
   :calibration-value
   :total-calibration-value
   :digits-in-line
   :all-calibration-values))

(in-package :day7)

(defun ints (line) 
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" line)))

(defun read-input (file)
  (with-open-file (f file)
    (loop for line = (read-line f nil nil)
          while line collect (ints line))))

(defun target-can-be-reached (equation)
  (let ((target (car equation))
        (numbers (cdr equation)))
    (labels ((rec (remaining current-total)
               (if (null remaining)
                   (= current-total target)
                   (when (<= current-total target)
                     (or (rec (cdr remaining) (* current-total (car remaining)))
                         (rec (cdr remaining) (+ current-total (car remaining))))))))
      (rec (cdr numbers) (car numbers)))))

(defun part1 ()
  (let ((equations (read-input "day7input")))
    (loop for equation in equations
          when (target-can-be-reached equation)
            sum (car equation))))

(defun number-length (n)
  (loop while (> n 0)
        for i from 1
        do (setf n (floor n 10))
        finally (return i)))

(defun concatenate-numbers (one other)
  (+ other (* one (expt 10 (number-length other)))))

(defun target-can-be-reached-with-concatenation (equation)
  (let ((target (car equation))
        (numbers (cdr equation)))
    (labels ((rec (remaining current-total)
               (if (null remaining)
                   (= current-total target)
                   (when (<= current-total target)
                     (or (rec (cdr remaining) (* current-total (car remaining)))
                         (rec (cdr remaining) (concatenate-numbers current-total (car remaining)))
                         (rec (cdr remaining) (+ current-total (car remaining))))))))
      (rec (cdr numbers) (car numbers)))))

(defun part2 ()
  (let ((equations (read-input "day7input")))
    (loop for equation in equations
          when (target-can-be-reached-with-concatenation equation)
            sum (car equation))))
