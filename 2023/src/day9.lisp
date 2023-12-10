(defpackage :day9
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind)
  (:export
   :next-number
   :previous-number))

(in-package :day9)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-int ()
  (sequential (negate (optional (char1 #\-)))
              (i *non-negative-int*)
              (if negate (- i) i)))

(defun parse-numbers ()
  (sep-by (parse-int) (many1 #p(char= #\space))))

(defun parse-sequences ()
  (sep-by (parse-numbers) (many1 #'newlinep)))

(defun read-sequences (filename)
  (parse-file filename (parse-sequences)))

(defun find-differences (sequence)
  (loop for remaining = sequence then (cdr remaining)
        while (cdr remaining)
        for first = (car remaining)
        for second = (cadr remaining)
        collect (- second first)))

(defun repeated-differences (sequence)
  (loop for current = sequence then (find-differences current)
        until (every #p(= 0) current)
        collect current))

(defun next-number (sequence)
  (reduce #'+ (mapcar #l(car (last %differences)) (repeated-differences sequence))))

(defun part1 ()
  (let ((sequences (read-sequences "input9")))
    (reduce #'+ (mapcar #'next-number sequences))))

(defun previous-number (sequence)
  (loop for d in (reverse (mapcar #'car (repeated-differences sequence)))
        for i = d then (- d i)
        finally (return i)))

(defun part2 ()
  (let ((sequences (read-sequences "input9")))
    (reduce #'+ (mapcar #'previous-number sequences))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
