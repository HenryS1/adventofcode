(defpackage :day6
  (:use :cl :iterate :pears :alexandria :anaphora :metabang-bind))

(in-package :day6)

(neat-lambda:enable-lambda-syntax)

(defun parse-line ()
  (parse-file "day6.input" (many #l(not (newlinep %line)))))

(defun find-start-of-packet (chars)
  (labels ((rec (i a b c d)
             (if (= (length (remove-duplicates (list a b c d))) 4)
                 (+ i 1)
                 (rec (+ i 1) b c d (aref chars (+ i 1))))))
    (rec 3 (aref chars 0) (aref chars 1) (aref chars 2) (aref chars 3))))

(defun part1 ()
  (find-start-of-packet (parse-line)))

(defun take (n l)
  (if (or (= n 0) (null l)) nil (cons (car l) (take (- n 1) (cdr l)))))

(defun find-start-of-message (chars)
  (labels ((rec (i seen)
             (if (= (length (remove-duplicates seen :test #'char=)) 14)
                 (+ i 1)
                 (rec (+ i 1) (cons (aref chars (+ i 1)) (butlast seen))))))
    (rec 13 (reverse (take 14 (coerce chars 'list))))))

(defun part2 ()
  (find-start-of-message (parse-line)))
