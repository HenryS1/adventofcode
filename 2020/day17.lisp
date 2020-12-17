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

(defpackage :day17
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day17)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input17" using #'read-line)
        (collect line)))

(defun read-space (lines extra-dimensions)
  (iter (with space = (make-hash-table :test 'equal))
        (for r from 0)
        (for row in lines)
        (iter (for c from 0)
              (for ch in-string row)
              (when (char= ch #\#)
                (setf (gethash (cons r (cons c extra-dimensions)) space) t)))
        (finally (return space))))

(defun process-neighbours (coord callback)
  (labels ((rec (rem acc)
             (if (null rem)
                 (let ((nbr (reverse acc)))
                   (when (not (equal coord nbr))
                     (funcall callback nbr)))
                 (progn (rec (cdr rem) (cons (car rem) acc))
                        (rec (cdr rem) (cons (+ (car rem) 1) acc))
                        (rec (cdr rem) (cons (- (car rem) 1) acc))))))
    (rec coord nil)))

(defun count-neighbours (coord space)
  (let ((total 0))
    (process-neighbours coord (lambda (nbr) (when (gethash nbr space) (incf total))))
    total))

(defun process-inactive-neighbours (coord space callback)
  (process-neighbours coord (lambda (nbr) (when (not (gethash nbr space))
                                            (funcall callback nbr)))))

(defun active-in-next-state (coord space)
  (let ((active-neighbours (count-neighbours coord space)))
    (or (and (gethash coord space) (= active-neighbours 2))
        (= active-neighbours 3))))

(defun determine-next-space (space)
  (iter (with next-space = (make-hash-table :test 'equal))
        (for (coord _) in-hashtable space)
        (when (active-in-next-state coord space)
          (setf (gethash coord next-space) t))
        (process-inactive-neighbours coord space
                                     (lambda (nbr) (when (active-in-next-state nbr space)
                                                     (setf (gethash nbr next-space) t))))
        (finally (return next-space))))

(defun evolve (turns lines extra-dimensions)
  (iter (for space first (read-space lines extra-dimensions) then (determine-next-space space))
        (for i from 1 to turns)
        (finally (return space))))

(defun part-1 () (evolve 6 (read-lines) '(0)))
(defun part-2 () (evolve 6 (read-lines) '(0 0)))
