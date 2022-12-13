(defpackage :aoc.datastructures
  (:use :cl :metabang-bind)
  (:export :make-queue :enqueue :enqueue-all :dequeue :q-empty))

(in-package :aoc.datastructures)

(defun make-queue (l)
  (cons (reverse l) nil))

(defun enqueue (e q)
  (bind (((back . front) q))
    (cons (cons e back) front)))

(defun enqueue-all (es q)
  (bind (((back . front) q))
    (cons (append (reverse es) back) front)))

(defun dequeue (q)
  (bind (((back . front) q))
    (if (null front)
        (let ((new-front (reverse back)))
          (cons (car new-front) (cons nil (cdr new-front))))
        (cons (car front) (cons back (cdr front))))))

(defun q-empty (q)
  (and (null (car q)) (null (cdr q))))
