(defpackage :day1
  (:use 
   :aoc.functional
   :cl 
   :cl-ppcre 
   :iterate 
   :alexandria 
   :anaphora 
   :metabang-bind))

(in-package :day1)

(defun read-lines ()
  (iter (for line in-file "day1.input" using #'read-line)
    (collect (parse-integer line :junk-allowed t))))

(defun group-elves (ns)
  (labels ((rec (rem)
             (when (not (null rem))
               (bind (((e . rest) (take-drop-while rem #'numberp)))
                 (cons e (rec (cdr rest)))))))
    (rec ns)))

(defun part1 ()
  (let ((lines (read-lines)))
    (reduce #'max (mapcar (lambda (e) (reduce #'+ e :initial-value 0)) 
                        (group-elves lines)) :initial-value 0)))

(defun part2 ()
  (let* ((lines (read-lines))
         (elves (mapcar (lambda (e) (reduce #'+ e)) (group-elves lines))))
    (reduce #'+ (take 3 (sort elves #'>)))))
