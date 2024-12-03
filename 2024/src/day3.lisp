(defpackage :day3
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

(in-package :day3)

(defun ints (line) 
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" line)))

(defun expressions (line) 
  (mapcar #'ints (cl-ppcre:all-matches-as-strings "mul\\(\\d{1,3},\\d{1,3}\\)" line)))

(defun evaluate-expression (expression)
  (* (car expression) (cadr expression)))

(defun read-input (file)
  (with-open-file (f file)
    (loop for line = (read-line f nil nil)
          while line collect (expressions line))))

(defun part1 ()
  (reduce #'+ (mapcar #'evaluate-expression (apply #'append (read-input "day3input")))))

(defun expressions-with-do (line)
  (cl-ppcre:all-matches-as-strings "(:?mul\\(\\d{1,3},\\d{1,3}\\)|do\\(\\)|don't\\(\\))" line))

(defun parse-expression (raw-expression)
  (cond ((string= raw-expression "do()") 'do)
        ((string= raw-expression "don't()") 'dont)
        (t (ints raw-expression))))

(defun input-with-do-and-dont (file)
  (with-open-file (f file)
    (loop for line = (read-line f nil nil)
          while line appending (mapcar #'parse-expression (expressions-with-do line)))))

(defun evaluate-expressions (expressions)
  (loop with enabled = t
        with total = 0
        for expression in expressions
        do (case expression
             (do (setf enabled t))
             (dont (setf enabled nil))
             (t (when enabled (incf total (evaluate-expression expression)))))
        finally (return total)))

(defun part2 ()
  (let ((expressions (input-with-do-and-dont "day3input")))
    (evaluate-expressions expressions)))
