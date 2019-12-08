(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day23
  (:use :cl :cl-ppcre :iterate :anaphora :metabang-bind :alexandria))

(in-package :day23)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defparameter *ip* 0)
(defparameter a 0)
(defparameter b 0)

(defun ip (i)
  (incf *ip* i))

(defmacro hlf (i)
  `(progn (setf ,i
               (floor ,i 2))
         (ip 1)))

(defmacro tpl (i)
  `(progn (setf ,i
                (* ,i 3))
          (ip 1)))

(defmacro inc (i)
  `(progn (setf ,i
                (incf ,i))
          (ip 1)))

(defun jmp (offset)
  (ip offset))

(defmacro jie (i offset)
  `(if (evenp ,i)
       (ip ,offset)
       (ip 1))) 

(defmacro jio (i offset)
  `(if (= ,i 1)
       (ip ,offset)
       (ip 1)))

(defun parse-instruction (line)
  (read-syms line ",?\\s+"))

(defun create-program (&optional (a-val 0))
  (let ((instructions (iter (for line in-file "input23" using #'read-line)
                            (for i from 0)
                            (collect `(,i ,(parse-instruction line))))))
    `(lambda () 
       (setf a ,a-val)
       (setf b 0)
       (setf *ip* 0)
       (iter outer
             (case *ip*
               ,@instructions
               (otherwise (return-from outer 
                            (progn 
                              (format t "~a~%" a)
                              (format t "~a~%" b)
                              b))))))))


(defun answer-1 () (funcall (eval (create-program))))

(defun answer-2 () (funcall (eval (create-program 1))))
