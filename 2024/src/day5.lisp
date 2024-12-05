(defpackage :day5
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

(in-package :day5)

(defun ints (line) 
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" line)))

(defun read-input (file)
  (let ((lines (with-open-file (f file)
                 (loop for line = (read-line f nil nil)
                       while line collect (ints line)))))
    (loop for rest on lines
          while (car rest)
          collect (car rest) into rules
          finally (return (cons rules (cdr rest))))))

(defun gather-rules (rules)
  (let ((befores (make-hash-table)))
    (loop for (before after) in rules
          do (push after (gethash before befores))
          finally (return befores))))

(defun in-correct-order (update befores)
  (loop with seen = (make-hash-table)
        for page in update
        do (setf (gethash page seen) t)
        when (some (lambda (other-page) (gethash other-page seen)) 
                   (gethash page befores))
          do (return nil)
        finally (return t)))

(defun middle-number (update)
  (let ((pages (length update)))
    (nth (floor pages 2) update)))

(defun part1 ()
  (let* ((input (read-input "day5input"))
         (befores (gather-rules (car input)))
         (updates (cdr input)))
    (loop for update in updates
          when (in-correct-order update befores)
            sum (middle-number update))))

(defun compare-by-before (befores)
  (lambda (one other)
    (find one (gethash other befores))))

(defun part2 ()
  (let* ((input (read-input "day5input"))
         (befores (gather-rules (car input)))
         (updates (cdr input))
         (comparator (compare-by-before befores)))
    (loop for update in updates
          when (not (in-correct-order update befores))
            sum (middle-number (sort update comparator)))))
