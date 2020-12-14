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

(defpackage :day13
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day13)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input13" using #'read-line)
        (collect line)))

(defun closest-departure (lines)
  (let* ((departure-goal (parse-integer (car lines)))
         (buses (ints (cadr lines)))
         (first (car (sort (mapcar #'cons buses 
                                   (mapcar (lambda (time) 
                                             (* (ceiling (/ departure-goal time)) time)) buses))
                           (lambda (one other) (< (cdr one) (cdr other)))))))
    (* (mod (cdr first) departure-goal) (car first))))

(defun departure-offsets (line)
  (iter (for str in (split "," line))
        (for offset from 1)
        (when (not (string= str "x"))
          (collect (cons (parse-integer str) offset)))))

(defun bezout (a b)
  (labels ((rec (a b skp skpp tkp tkpp)
             (bind (((:values q r) (floor a b))
                    (sk (- skpp (* q skp)))
                    (tk (- tkpp (* q tkp))))
               (if (= r 0)
                   (list b skp tkp)
                   (rec b r sk skp tk tkp)))))
    (rec a b 0 1 1 0)))

(defun chinese-remainder (departures)
  (iter (with m = (reduce #'* (mapcar #'car departures)))
        (for (mi . offset) in departures)
        (for ci = (- mi offset))
        (for ni = (floor m mi))
        (for (d inv _) = (bezout ni mi))
        (summing (* ci ni inv) into x)
        (finally (return (mod (+ x m) m)))))

(defun part-1 ()
  (closest-departure (read-lines)))

(defun part-2 ()
  (chinese-remainder (departure-offsets (cadr (read-lines)))))
