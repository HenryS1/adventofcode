(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-ppcre)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind)
  (ql:quickload :alexandria)
  (ql:quickload :trivia)
  (ql:quickload :trivia.ppcre)
  (load "../2018/queue.lisp")
  (load "../2018/priority-queue.lisp")) 

(defpackage :day4
  (:use :cl :cl-ppcre :trivia trivia.ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day4)

(defun read-lines ()
  (iter (for line in-file "day4.input" using #'read-line)
    (collect (match line
                 ((ppcre "(\\d+)-(\\d+),(\\d+)-(\\d+)" 
                         (read start1) (read end1) (read start2) (read end2))
                  (cons (cons start1 end1) (cons start2 end2)))))))

(defun contains (one other)
  (bind (((start1 . end1) one)
         ((start2 . end2) other))
    (<= start1 start2 end2 end1)))

(defun fully-overlaps (one other)
  (or (contains one other) (contains other one)))

(defun count-full-overlaps (lines)
  (length (remove-if-not (lambda (elves) 
                           (bind (((one . other) elves)) (fully-overlaps one other)))
                         lines)))

(defun part1 ()
  (count-full-overlaps (read-lines)))

(defun overlaps (one other)
  (bind (((start1 . end1) one)
         ((start2 . end2) other))
    (or (<= start1 start2 end1)
        (<= start1 end2 end1)
        (<= start2 start1 end2)
        (<= start2 end1 end2))))

(defun count-overlaps (lines)
  (length (remove-if-not (lambda (elves)
                           (bind (((one . other) elves)) (overlaps one other)))
                         lines)))

(defun part2 ()
  (count-overlaps (read-lines)))
