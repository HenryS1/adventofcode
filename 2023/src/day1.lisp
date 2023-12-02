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

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-lines ()
  (parse-file "input1" (lines (many1 #'alphanumericp))))

(defun calibration-value (line)
  (let* ((only-numbers (remove-if-not #'digit-char-p line)))
    (+ (* 10 (digit-char-p (aref only-numbers 0))) 
       (digit-char-p (aref only-numbers (- (length only-numbers) 1))))))

(defun total-calibration-value (lines)
  (reduce #l(+ %1v (calibration-value %2line)) lines :initial-value 0))

(defun part1 ()
  (total-calibration-value (read-lines)))

(defun find-digit-at (i str digit n)
  (when (string-equal str digit :start1 i :end1 (min (length str) (+ i (length digit)))) n))

(defun digit-at (i str)
  (acond 
    ((find-digit-at i str "one" 1) it)
    ((find-digit-at i str "two" 2) it)
    ((find-digit-at i str "three" 3) it)
    ((find-digit-at i str "four" 4) it)
    ((find-digit-at i str "five" 5) it)
    ((find-digit-at i str "six" 6) it)
    ((find-digit-at i str "seven" 7) it)
    ((find-digit-at i str "eight" 8) it)
    ((find-digit-at i str "nine" 9) it)
    ((digit-char-p (aref str i)) it)
    (t nil)))

(defun digits-in-line (line)
  (loop for i from 0 to (- (length line) 1) for d = (digit-at i line) when d collect d))

(defun all-calibration-values ()
  (sep-by (fmap #l(+ (* 10 (first %digits)) (car (last %digits)))
                (fmap #'digits-in-line (many #'alphanumericp)))
          (one #'newlinep)))

(defun read-calibration-values ()
  (parse-file "input1" (all-calibration-values)))

(defun part2 ()
  (reduce #p(+) (read-calibration-values)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
