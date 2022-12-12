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

(defpackage :day1
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day1)

(defun read-lines ()
  (iter (for line in-file "day1.input" using #'read-line)
    (collect (parse-integer line :junk-allowed t))))

(defun take-drop (ls p)
  (labels ((rec (acc rest)
             (cond ((null rest) (cons (reverse acc) rest))
                   ((funcall p (car rest))
                    (rec (cons (car rest) acc) (cdr rest)))
                   (t (cons (reverse acc) rest)))))
    (rec nil ls)))

(defun group-elves (ns)
  (labels ((rec (rem)
             (when (not (null rem))
               (bind (((e . rest) (take-drop rem #'numberp)))
                 (cons e (rec (cdr rest)))))))
    (rec ns)))

(defun part1 ()
  (let ((lines (read-lines)))
    (reduce #'max (mapcar (lambda (e) (reduce #'+ e :initial-value 0)) 
                        (group-elves lines)) :initial-value 0)))

(defun take (n l)
  (if (or (null l) (= n 0)) nil (cons (car l) (take (- n 1) (cdr l)))))

(defun part2 ()
  (let* ((lines (read-lines))
         (elves (mapcar (lambda (e) (reduce #'+ e)) (group-elves lines))))
    (reduce #'+ (take 3 (sort elves #'>)))))
