(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day16
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day16)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-digits ()
  (with-open-file (f "input16") 
    (map 'vector #'digit-char-p (read-line f))))

(defun fft (sequence pattern digit)
  (iter outer
        (for i first 0 then (+ i (* digit (length pattern))))
        (while (< i (length sequence)))
        (iter (for j from 0 to (- (length pattern) 1))
              (while (< (+ i (* j digit)) (length sequence)))
              (iter (for k from 0 to (- digit 1))
                    (for ind = (+ i (* j digit) k))
                    (while (< ind (length sequence)))
                    (in outer (sum (* (aref sequence ind) 
                                      (aref pattern (mod (+ j (if (> (+ k 2)
                                                                            digit)
                                                                         1 0))
                                                         (length pattern))))
                                   into result))))
        (finally (return-from outer (mod (abs result) 10)))))

(defun run-phase (sequence pattern)
  (iter (with next = (make-array (length sequence)))
        (for digit from 1 to (length sequence))
        (setf (aref next (- digit 1)) (fft sequence pattern digit))
        (finally (return next))))

(defun answer-1 ()
  (iter (for i from 0 to 100)
        (for sequence first (read-digits) 
             then (run-phase sequence (vector 0 1 0 -1)))
        (finally (return sequence))))

(defun repeat-digits (ar times)
  (map 'vector #'identity 
       (iter (with l = (coerce ar 'list))
             (for i from 1 to times)
             (appending l))))

(defun calc-row (len)
  (iter (with init = (make-array len :initial-element 1))
        (for i from 1 to 99)
        (iter (for j from 1 to (- len 1))
              (incf (aref init j) (aref init (- j 1))))
        (finally (return init))))

(defun find-offset (digits)
  (parse-integer (map 'string (lambda (c) (aref (format nil "~a" c) 0)) (subseq digits 0 7))))

(defun mult-sum (first-row sequence start)
  (iter (for i from start to (- (length sequence) 1))
        (for j from 0)
        (sum (* (aref first-row j) (aref sequence i)))))

(defun answer-2 ()
  (let* ((digits (read-digits))
         (offset (find-offset digits))
         (first-row (calc-row (- (* (length digits) 10000) offset)))
         (sequence (subseq (repeat-digits digits 10000) (- offset 1))))
    (iter (for i from 1 to 8)
          (collect (mod (mult-sum first-row sequence i) 10)))))
