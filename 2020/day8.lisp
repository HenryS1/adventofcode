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

(defpackage :day8
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day8)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input8" using #'read-line)
        (collect line)))

(defun read-instruction (line)
  (match line
    ((ppcre "(\\w+)\\s+\\\+?(-?\\d+)" (read sym) (read n))
     (list sym n))))

(defun read-program (lines)
  (map 'vector #'read-instruction lines))

(defun find-loop-acc (program)
  (iter (with seen = (make-hash-table))
        (with acc = 0)
        (with i = 0)
        (when (or (>= i (length program)) (< i 0) (gethash i seen))
          (return (list acc i))) 
        (for (sym n) = (aref program i))
        (setf (gethash i seen) t)
        (case sym
          (nop (incf i))
          (acc (incf acc n) (incf i))
          (jmp (incf i n)))))

(defun toggle-ins (ins)
  (case ins 
    (jmp 'nop)
    (nop 'jmp)))

(defun find-exit (lines)
  (iter (with program = (read-program lines))
        (for j from 0 to (- (length program) 1))
        (for ins = (aref program j))
        (when (or (eq (car ins) 'jmp)
                  (eq (car ins) 'nop))
          (setf (car ins) (toggle-ins (car ins)))
          (for (acc end) = (find-loop-acc program))
          (when (= end (length program))
            (return (list acc end)))
          (setf (car ins) (toggle-ins (car ins))))))

(defun part-1 () (-> (read-lines) (read-program) (find-loop-acc) (car)))

(defun part-2 () (-> (read-lines) (find-exit) (car)))
