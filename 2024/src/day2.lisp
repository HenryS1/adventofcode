(defpackage :day2
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

(in-package :day2)

(defun ints (line) 
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" line)))

(defun read-input (file)
  (with-open-file (f file)
    (loop for line = (read-line f nil nil)
          while line collect (ints line))))

(defun is-gradually-increasing-or-decreasing (report)
  (loop with sign = (signum (- (cadr report) (car report)))
        for rest on report
        while (cdr rest)
        for one = (car rest)
        for other = (cadr rest)
        for current-sign = (signum (- other one))
        when (or (/= current-sign sign)
                 (not (<= 1 (abs (- other one)) 3)))
        do (return nil)
        finally (return t)))

(defun part1 ()
  (let ((reports (read-input "day2input")))
    (length (remove-if-not #'is-gradually-increasing-or-decreasing reports))))
 
(defun is-safe-when-dampened (report)
  (labels ((rec (rest removed sign)
             (or (null (cdr rest))
                 (let* ((one (car rest))
                        (other (cadr rest))
                        (current-sign (signum (- other one))))
                   (and (not (or (/= current-sign sign)
                                 (not (<= 1 (abs (- other one)) 3))))
                        (or (rec (cdr rest) removed sign)
                            (when (not removed) (rec (cons (cadr rest) (cdddr rest)) t sign))))))))
    (or (rec report nil (signum (- (cadr report) (car report))))
        (rec (cdr report) t (signum (- (caddr report) (cadr report))))
        (rec (cons (car report) (cddr report)) t (signum (- (caddr report) (car report)))))))

(defun part2 ()
  (let ((reports (read-input "day2input")))
    (length (remove-if-not #'is-safe-when-dampened reports))))
