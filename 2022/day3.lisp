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

(defpackage :day3
  (:use :cl :cl-ppcre :trivia trivia.ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day3)

(defun priority (e)
  (loop for i from 1
        for c across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        until (char= c e)
        finally (return i)))

(defun read-lines ()
  (iter (for line in-file "day3.input" using #'read-line)
    (collect line)))

(defun split-in-half (items)
  (cons (map 'list #'identity (subseq items 0 (floor (length items) 2)))
        (map 'list #'identity (subseq items (floor (length items) 2)))))

(defun occurring-in-both (items)
  (bind (((fst . snd) (split-in-half items)))
    (remove-duplicates (intersection fst snd))))

(defun part1 ()
  (reduce #'+ (mapcar (lambda (ps) (reduce #'+ ps))
                      (mapcar (lambda (es) (mapcar #'priority es)) 
                              (mapcar #'occurring-in-both (read-lines))))))

(defun take-drop (n ls)
  (labels ((rec (cur-n acc rest)
             (if (or (null rest) (= cur-n 0)) 
                 (cons (reverse acc) rest)
                 (rec (- cur-n 1) (cons (car rest) acc) (cdr rest)))))
    (rec n nil ls)))

(defun groups-of-3 (items)
  (when items
    (bind (((grp . rest) (take-drop 3 items)))
      (cons grp (groups-of-3 rest))))) 

(defun find-badge (ls)
  (let ((groups (mapcar (lambda (items) (map 'list #'identity items)) ls)))
    (remove-duplicates (intersection (car groups) (intersection (cadr groups) (caddr groups))))))

(defun part2 ()
  (reduce #'+ (mapcar #'priority (flatten (mapcar #'find-badge (groups-of-3 (read-lines)))))))
