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

(defpackage :day22
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day22)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input22" using #'read-line)
        (collect line)))

(defun read-players (lines)
  (iter (for rem first lines then (cdr rem))
        (while rem)
        (for line = (car rem))
        (match line 
          ((ppcre "Player \\d:")
           (bind (((deck . next-rem) (read-player (cdr rem))))
             (setf rem next-rem)
             (collect deck))))))

(defun read-player (lines)
  (iter (for rem first lines then (cdr rem))
        (while rem)
        (for line = (car rem))
        (while (> (length line) 0))
        (collect (parse-integer line) into deck)
        (finally (return (cons deck rem)))))

(defun play-game (lines)
  (bind ((*print-circle* t)
         ((player1 player2) (read-players lines))
         (total-length (+ (length player1) (length player2)))
         (e1 (last player1))
         (e2 (last player2))) 
    (setf (cdr e1) player1)
    (setf (cdr e2) player2)
    (iter (while (and player1 player2))
          (for round from 1)
          (for c1 = (car player1))
          (for c2 = (car player2))
          (format t "round ~a~%p1 ~a~%p2 ~a~%c1 ~a c2 ~a~%" round player1 player2 c1 c2)
          (if (> c1 c2)
              (progn (setf (cdr player1) (cons c2 (cdr player1)))
                     (setf (cdr e1) player1)
                     (setf player1 (cddr player1))
                     (setf e1 (cddr e1))
                     (if (= (car e2) (cadr e2))
                         (progn (setf e2 nil)
                                (setf player2 nil))
                         (progn (setf (cdr e2) (cddr e2))
                                (setf player2 (cdr player2)))))
              (progn (setf (cdr player2) (cons c1 (cdr player2)))
                     (format t "player2 ~a~%" player2)
                     (setf (cdr e2) player2)
                     (format t "e2 ~A~%" e2)
                     (format t "player2 again ~a~%" player2)
                     (setf player2 (cddr player2))
                     (format t "player2 last time ~a~%" player2)
                     (setf e2 (cddr e2))
                     (if (= (car e1) (cadr e1))
                         (progn (setf e1 nil)
                                (setf player1 nil))
                         (progn (setf (cdr e1) (cddr e1))
                                (setf player1 (cdr player1))))
                     (format t "e1 ~a~%" e1))))
    (iter (for i from 0 to (- total-length 1))
          (for c in (if player1 player1 player2))
          (format t "~a * ~a~%" c (- total-length i))
          (summing (* c (- total-length i))))))
