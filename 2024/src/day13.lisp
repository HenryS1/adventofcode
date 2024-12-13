(defpackage :day13
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :metabang-bind)
  (:export 
   :calibration-value
   :total-calibration-value
   :digits-in-line
   :all-calibration-values))

(in-package :day13)

(defun ints (line) 
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" line)))

(defstruct claw (ax 0 :type fixnum) (ay 0 :type fixnum) (bx 0 :type fixnum) (by 0 :type fixnum)
           (px 0 :type fixnum) (py 0 :type fixnum))

(defun parse-claw (stream)
  (let ((a-raw (read-line stream nil nil)))
    (when (and a-raw (> (length a-raw) 0))
      (let ((a (ints a-raw))
            (b (ints (read-line stream)))
            (prize (ints (read-line stream))))
        (make-claw :ax (car a) :ay (cadr a) :bx (car b) :by (cadr b) 
                   :px (car prize) :py (cadr prize))))))

(defun read-input (file)
  (with-open-file (f file)
    (loop for claw = (parse-claw f)
          while claw
          for ignored = (read-line f nil nil)
          collect claw)))

(defun find-a-solution (ax bx px)
  (loop for a from 0 
        for a-part = (* a ax)
        while (<= a-part px)
        for b = (floor (- px a-part) bx)
        for b-part = (* b bx)
        for total = (+ a-part b-part)
        until (= total px)
        finally (return (cons a b))))

(defun earn-prize-bin-search (claw)
  (let ((ax (claw-ax claw))
        (ay (claw-ay claw))
        (bx (claw-bx claw))
        (by (claw-by claw))
        (px (claw-px claw))
        (py (claw-py claw)))
    (let ((dx (gcd ax bx))
          (dy (gcd ay by)))
      (when (and (= (mod px dx) 0)
                 (= (mod py dy) 0))
       (let* ((ax1 (floor ax dx))
              (bx1 (floor bx dx))
              (an-x-solution (find-a-solution ax bx px))
              (a-sol (car an-x-solution))
              (b-sol (cdr an-x-solution)))
         (when an-x-solution
           (loop with k-lower = 0
                 with k-upper = (floor b-sol ax1)
                 with a-diff = (* bx1 ay)
                 with b-diff = (* ax1 by)
                 for k = (floor (+ k-lower k-upper) 2)
                 for a = (+ a-sol (* k bx1))
                 for b = (- b-sol (* k ax1))
                 for ay-part = (* a ay)
                 for by-part = (* b by)
                 for total = (+ ay-part by-part)
                 if (= total py)
                   do (return (list a b (+ (* a 3) b)))
                 else if (<= k-upper k-lower)
                        do (return nil)
                 else if (< total py)
                        do (if (< a-diff b-diff)
                               (setf k-upper (floor (+ k-lower k-upper) 2))
                               (setf k-lower (+ (floor (+ k-lower k-upper) 2) 1)))
                 else do (if (< a-diff b-diff)
                             (setf k-lower (+ (floor (+ k-lower k-upper) 2) 1))
                             (setf k-upper (floor (+ k-lower k-upper) 2))))))))))

(defun part1 ()
  (let ((claws (read-input "day13input")))
    (loop for claw in claws
          for i from 0
          for (b-a b-b b-cost) = (earn-prize-bin-search claw)
          for (best-a best-b best-cost) = (earn-prize-bin-search claw)
          when best-cost
            sum i into possible
            and sum best-a into as
            and sum best-b into bs
            and sum best-cost into best-total
          finally (return (list possible as bs best-total)))))

(defun part2 ()
  (let ((claws (read-input "day13input")))
    (mapc (lambda (claw) 
            (incf (claw-px claw) 10000000000000)
            (incf (claw-py claw) 10000000000000)) claws)
    (loop for claw in claws
          for i from 0
          for (best-a best-b best-cost) = (earn-prize-bin-search claw)
          when best-cost
            sum best-a into as
            and sum best-b into bs
            and sum best-cost into best-total
          finally (return (list as bs best-total)))))

