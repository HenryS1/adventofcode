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
                     (or (rec (cdr remaining) (+ current-total (car remaining)))
                         (rec (cdr remaining) (* current-total (car remaining))))))))
      (rec (cdr numbers) (car numbers)))))

(defun part1 ()
  (let ((equations (read-input "day7input")))
    (loop for equation in equations
          when (target-can-be-reached equation)
            sum (car equation))))

(declaim (inline number-shift))
(defun number-shift (n)
  (declare (fixnum n) (optimize (speed 3)))
  (loop while (> n 0)
        for i of-type (unsigned-byte 16) = 10 then (* 10 i)
        do (setf n (floor n 10))
        finally (return i)))

(declaim (inline concatenate-numbers))
(defun concatenate-numbers (one other)
  (declare (optimize (speed 3)) (fixnum one other))
  (the fixnum (+ (the fixnum (* (number-shift other) one)) other)))

(defun target-can-be-reached-with-concatenation (equation)
  (declare (optimize (speed 3)))
  (let ((target (car equation))
        (numbers (cdr equation)))
    (declare (fixnum target))
    (labels ((rec (remaining current-total)
               (declare (fixnum current-total))
               (if (null remaining)
                   (= current-total target)
                   (when (<= current-total target)
                     (let ((next (car remaining))
                           (rest (cdr remaining)))
                       (declare (fixnum next))
                       (or (rec rest (* current-total next))
                           (when (<= (the fixnum (* current-total (number-shift next))) target)
                             (rec rest (concatenate-numbers current-total next)))
                           (rec rest (+ current-total next))))))))
      (rec (cdr numbers) (car numbers)))))

(defun part2 ()
  (declare (optimize (speed 3)))
  (let ((equations (read-input "day7input")))
    (loop with result fixnum = 0
          for equation in equations
          when (target-can-be-reached-with-concatenation equation)
            do (setf result (the fixnum (+ result (the fixnum (car equation)))))
          finally (return result))))
