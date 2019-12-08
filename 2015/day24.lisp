(ql:quickload :cl-ppcre)
(ql:quickload :metabang-bind)
(ql:quickload :iterate)
(ql:quickload :alexandria)
(ql:quickload :anaphora)

(defpackage :day24
  (:use :cl :cl-ppcre :metabang-bind :iterate :alexandria :anaphora))

(in-package :day24)

(defun read-is ()
  (iter (for line in-file "input24" using #'read-line)
        (collect (parse-integer line))))

(defparameter *max-int* (floor (- (expt 2 31) 1) 1))

(defun find-candidates (is)
  (let ((results (list))
        (target (/ (reduce #'+ is) 3))
        (min-length (length is))
        (min-qe *max-int*))
    (labels ((rec (rem g1 g2 g3)
               (cond ((and (null rem) 
                           (or (< (length g1) min-length)
                               (and (<= (length g1) min-length) 
                                    (< (reduce #'* (cdr g1)) min-qe))))
                      (setf min-length (length g1))
                      (setf min-qe (reduce #'* (cdr g1)))
                      (push (list g1 g2 g3) results))
                     ((null rem) nil)
                     (t (when (or (null g1) (and (<= (+ (car rem) (car g1)) target)
                                                 (or (< (length g1) min-length)
                                                     (and (<= (length g1) min-length)
                                                          (< (reduce #'* (cdr g1))
                                                             min-qe)))))
                          (rec (cdr rem) (cons (+ (or (car g1) 0) (car rem))
                                               (cons (car rem) (cdr g1)))
                               g2 g3))
                        (when (or (null g2) (and (<= (+ (car rem) (car g2)) target)
                                                 (or (< (length g1) min-length)
                                                     (and (<= (length g1) min-length)
                                                          (< (reduce #'* (cdr g1))
                                                             min-qe)))))
                          (rec (cdr rem) g1
                               (cons (+ (or (car g2) 0) (car rem))
                                     (cons (car rem) (cdr g2)))
                               g3))
                        (when (or (null g3) (and (<= (+ (car rem) (car g3)) target)
                                                 (or (< (length g1) min-length)
                                                     (and (<= (length g1) min-length)
                                                          (< (reduce #'* (cdr g1))
                                                             min-qe)))))
                          (rec (cdr rem) g1 g2
                               (cons (+ (or (car g3) 0) (car rem))
                                     (cons (car rem) (cdr g3)))))))))
      (rec (cdr is) (list (car is) (car is)) (list) (list))
      (reduce #'* (cdr (caar results))))))

(defun answer-1 ()
  (find-candidates (reverse (read-is))))
