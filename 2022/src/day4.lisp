(defpackage :day4
  (:use :cl  :iterate :alexandria :anaphora :metabang-bind :pears))

(in-package :day4)

(neat-lambda:enable-lambda-syntax)

(defun parse-section ()
  (sequential (start *positive-int*)
              (_ (char1 #\-))
              (end *positive-int*)
              (cons start end)))

(defun parse-lines ()
  (parse-file "day4.input" 
              (sep-by (sequential (fst (parse-section))
                                  (_ (char1 #\,))
                                  (snd (parse-section))
                                  (cons fst snd))
                      (char1 #\newline))))

(defun contains (one other)
  (bind (((start1 . end1) one)
         ((start2 . end2) other))
    (<= start1 start2 end2 end1)))

(defun fully-overlaps (one other)
  (or (contains one other) (contains other one)))

(defun count-full-overlaps (lines)
  (length (remove-if-not #l(bind (((one . other) %elves)) (fully-overlaps one other))
                         lines)))

(defun part1 ()
  (count-full-overlaps (parse-lines)))

(defun overlaps (one other)
  (bind (((start1 . end1) one)
         ((start2 . end2) other))
    (or (<= start1 start2 end1)
        (<= start1 end2 end1)
        (<= start2 start1 end2)
        (<= start2 end1 end2))))

(defun count-overlaps (lines)
  (length (remove-if-not #l(bind (((one . other) %elves)) (overlaps one other))
                         lines)))

(defun part2 ()
  (count-overlaps (parse-lines)))
