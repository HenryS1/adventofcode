(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :ironclad)
(ql:quickload :flexi-streams)
(ql:quickload :anaphora)

(defpackage :day2
  (:use :cl :cl-ppcre :iterate :ironclad :flexi-streams :anaphora))

(in-package :day2)

(defun find-hash (num)
  (let* ((input (format nil "~a" num)))
    (byte-array-to-hex-string (digest-sequence :md5 (string-to-octets input)))))

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "\\d+" line)))

(defun read-syms (line)
  (let (*read-eval*)
    (with-input-from-string (s line)
      (iter (for sym in-stream s)
            (collect sym)))))

(defun find-termination (in1 in2 codes)
  (setf (aref codes 1) in1)
  (setf (aref codes 2) in2)
  (iter (for i first 3 then (+ i 4))
        (while (< (- i 3) (length codes)))
        (for i1 = (aref codes (- i 3)))
        (when (= i1 99) (return-from find-termination (aref codes 0)))
        (for i4 = (aref codes i))
        (for i3 = (aref codes (- i 1)))
        (for i2 = (aref codes (- i 2)))
        (case i1 
          (1 (setf (aref codes i4) (+ (aref codes i3) (aref codes i2))))
          (2 (setf (aref codes i4) (* (aref codes i3) (aref codes i2)))))))

(defun answer-1 ()
  (iter (for line in-file "input2" using #'read-line)
        (for codes = (map 'vector #'identity (ints line)))
        (finally (return (find-termination 12 2 codes)))))

(defun answer-2 ()
  (iter (for line in-file "input2" using #'read-line)
        (iter (for in1 from 0 to 99)
              (iter (for in2 from 0 to 99)
                    (for codes = (map 'vector #'identity (ints line)))
                    (when (= (find-termination in1 in2 codes) 19690720)
                      (return-from answer-2 (+ (* 100 in1) in2)))))))
