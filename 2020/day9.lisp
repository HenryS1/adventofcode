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

(defpackage :day9
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day9)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input9" using #'read-line)
        (collect line)))

(defun nums () (map 'vector #'parse-integer (read-lines)))

(defun check-i (nums i offset)
  (iter (with n = (aref nums i))
        (for j from (- i offset) to (- i 2))
        (awhen (iter (for k from (+ (- i offset) 1) to (- i 1))
                    (when (= (+ (aref nums j) (aref nums k)) n)
                      (return (list (aref nums j) (aref nums k)))))
          (return it))
        (finally (return nil))))

(defun check-sums (nums offset)
  (iter (for i from offset to (- (length nums) 1))
        (when (not (check-i nums i offset))
          (return (aref nums i)))))

(defun find-sum (nums target)
  (iter (for i from 0 to (- (length nums) 2))
        (for (total j) = (iter (for j from i to (- (length nums) 1))
                               (for total first (aref nums j) then (+ total (aref nums j)))
                               (until (>= total target))
                               (finally (return (list total j)))))
        (when (= total target)
          (return (iter (for k from i to j)
                        (maximizing (aref nums k) into max)
                        (minimizing (aref nums k) into min)
                        (finally (return (list (+ max min) i j))))))))
