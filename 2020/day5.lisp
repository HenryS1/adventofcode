(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :trivia)
  (ql:quickload :trivia.ppcre)
  (ql:quickload :cl-ppcre)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind)
  (ql:quickload :alexandria)
  (ql:quickload :cl-arrows)
  (load "../2018/queue.lisp")
  (load "../2018/priority-queue.lisp"))

(defpackage :day5
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day5)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input5" using #'read-line)
        (collect line)))

(defun decode (code min max toggle)
  (iter (for c in-string code)
        (if (char= c toggle)
            (setf min (+ (floor (+ max min) 2) 1))
            (setf max (floor (+ max min) 2)))
        (finally (return (values min max)))))

(defun decode-seat (line)
  (bind ((row-code (subseq line 0 7))
         (col-code (subseq line 7)))
    (values (decode row-code 0 127 #\B) (decode col-code 0 7 #\R))))

(defun seat-id (line)
  (multiple-value-bind (row col) (decode-seat line)
    (+ (* row 8) col)))

(defun max-seat-id (lines)
  (iter (for line in lines)
        (for (row col) = (multiple-value-list (decode-seat line)))
        (maximizing (+ (* row 8) col))))

(defun find-available-seat (lines)
  (iter (with seat-ids = (sort (mapcar #'seat-id lines) #'<))
        (for s in seat-ids)
        (for sp previous s)
        (when (and sp (= sp (- s 2)))
          (return (- s 1)))))
