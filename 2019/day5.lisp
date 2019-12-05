(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :ironclad)
(ql:quickload :trivia)
(ql:quickload :flexi-streams)

(defpackage :day5
  (:use :cl :cl-ppcre :iterate :anaphora :ironclad :trivia :flexi-streams))

(in-package :day5)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun find-hash (num)
  (let* ((input (format nil "~a" num)))
    (byte-array-to-hex-string (digest-sequence :md5 (string-to-octets input)))))

(defun eval-value (mode v codes)
  (case mode
    (0 (aref codes v))
    (1 v)))

(defun digit (n i)
  (mod (floor n (expt 10 i)) 10))

(defun binop (ind codes op)
  (let* ((encoding (aref codes ind))
         (m1 (digit encoding 2))
         (m2 (digit encoding 3))
         (v1 (eval-value m1 (aref codes (+ ind 1)) codes))
         (v2 (eval-value m2 (aref codes (+ ind 2)) codes))
         (dest (aref codes (+ ind 3))))
    (format t "M1 ~a M2 ~a ARG1 ~a ARG2 ~a DEST ~a~%" m1 m2 v1 v2 dest)
    (setf (aref codes (aref codes (+ ind 3)))
          (funcall op v1 v2))
    (format t "RESULT ~a~%" (aref codes (aref codes (+ ind 3))))
    (+ ind 4)))

(defun mult (ind codes)
  (binop ind codes #'*))

(defun plus (ind codes)
  (binop ind codes #'+))

(defun save (inp ind codes)
  (progn (setf (aref codes (aref codes (+ ind 1))) inp)
         (+ ind 2)))

(defun output (ind codes)
  (let* ((encoding (aref codes ind))
         (mode (digit encoding 2)))
    (format t "~a~%" (eval-value mode (aref codes (+ ind 1)) codes))
    (+ ind 2)))

(defun find-termination (in codes)
  (iter (with i = 0)
        (for code = (aref codes i))
        (format t "CODE ~a~%" code)
        (case (mod code 100)
          (99 (return-from find-termination nil))
          (1 (setf i (plus i codes)))
          (2 (setf i (mult i codes)))
          (3 (setf i (save in i codes)))
          (4 (setf i (output i codes))))))

(defun answer-1 ()
  (let ((codes (with-open-file (f "input5") 
                 (map 'vector #'identity (ints (read-line f))))))
    (find-termination 1 codes)))
