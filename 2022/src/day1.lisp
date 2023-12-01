(defpackage :day1
  (:use 
   :aoc.functional
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind))

(in-package :day1)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-elves ()
  (sep-by (sep-by *positive-int* (char1 #\newline))
                (manyn #'newlinep 2)))

(defun read-elves ()
  (parse-file "day1.input" (parse-elves)))

(defun part1 ()
  (reduce #'max (mapcar #l(reduce #'+ %elf :initial-value 0) 
                        (read-elves)) :initial-value 0))

(defun part2 ()
  (let* ((elves (mapcar #p(reduce #'+) (read-elves))))
    (reduce #'+ (take 3 (sort elves #'>)))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
