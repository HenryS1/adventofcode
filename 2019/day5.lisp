(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :trivia)
(ql:quickload :alexandria)

(defpackage :day5
  (:use :cl :cl-ppcre :iterate 
        :anaphora :trivia :alexandria))

(in-package :day5)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun eval-value (mode v codes)
  (case mode
    (0 (aref codes v))
    (1 v)))

(defun digit (n i)
  (mod (floor n (expt 10 i)) 10))

(defmacro with-params (ind codes &rest body)
  (with-gensyms (encoding m1 m2)
    `(let* ((,encoding (aref ,codes ,ind))
            (,m1 (digit ,encoding 2))
            (,m2 (digit ,encoding 3))
            (v1 (eval-value ,m1 (aref ,codes (+ ,ind 1)) ,codes))
            (v2 (eval-value ,m2 (aref ,codes (+ ,ind 2)) ,codes)))
       ,@body)))

(defun binop (ind codes op)
  (with-params ind codes
     (setf (aref codes (aref codes (+ ind 3)))
           (funcall op v1 v2))
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

(defun jit (ind codes)
  (with-params ind codes
               (if (/= v1 0) v2 (+ ind 3))))

(defun jif (ind codes)
  (with-params ind codes
               (if (= v1 0) v2 (+ ind 3))))

(defun lt (ind codes)
  (with-params ind codes
   (let ((dest (aref codes (+ ind 3))))
     (if (< v1 v2) (setf (aref codes dest) 1) (setf (aref codes dest) 0))
     (+ ind 4))))

(defun equals (ind codes)
  (with-params ind codes
    (let ((dest (aref codes (+ ind 3))))
     (if (= v1 v2) (setf (aref codes dest) 1) (setf (aref codes dest) 0))
     (+ ind 4))))

(defun find-termination (in codes)
  (iter (with i = 0)
        (for code = (aref codes i))
        (case (mod code 100)
          (99 (return-from find-termination nil))
          (1 (setf i (plus i codes)))
          (2 (setf i (mult i codes)))
          (3 (setf i (save in i codes)))
          (4 (setf i (output i codes)))
          (5 (setf i (jit i codes)))
          (6 (setf i (jif i codes)))
          (7 (setf i (lt i codes)))
          (8 (setf i (equals i codes))))))

(defun answer-1 ()
  (let ((codes (with-open-file (f "input5") 
                 (map 'vector #'identity (ints (read-line f))))))
    (find-termination 1 codes)))

(defun answer-2 ()
  (let ((codes (with-open-file (f "input5")
                 (map 'vector #'identity (ints (read-line f))))))
    (find-termination 5 codes)))
