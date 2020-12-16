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

(defpackage :day16
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day16)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input16" using #'read-line)
        (collect line)))

(defun read-rules-and-tickets (lines)
  (iter (with mine = nil)
        (for rem first lines then (cdr rem))
        (while rem)
        (for line = (car rem))
        (match line
          ((ppcre "([^\\s:]+)[^\\d]+(\\d+)-(\\d+) or (\\d+)-(\\d+)" 
                  (read prefix) (read min1) (read max1) (read min2) (read max2))
           (collect (list prefix min1 max1 min2 max2) into rules result-type 'vector))
          ((ppcre "your ticket")
           (setf mine (coerce (ints (cadr rem)) 'vector))
           (setf rem (cddr rem)))
          ((ppcre "\\d+,") (collect (ints line) into tickets)))
        (finally (return (list mine rules tickets)))))

(defun count-invalid-values (ticket rules)
  (iter (for v in ticket)
        (when (not (iter (for rule in-vector rules)
                         (match rule 
                           ((guard (list _ min1 max1 min2 max2)
                                   (or (<= min1 v max1) (<= min2 v max2)))
                            (return t)))
                         (finally (return nil))))
          (sum v))))

(defun count-all-invalid (lines)
  (bind (((_ rules tickets) (read-rules-and-tickets lines)))
    (iter (for ticket in tickets)
          (sum (count-invalid-values ticket rules)))))

(defun initialise-impossible (rules)
  (let ((impossible (make-array (length rules))))
    (iter (for rule in-vector rules)
          (for i from 0)
          (setf (aref impossible i) (make-hash-table)))
    impossible))

(defun check-impossible (r all-impossible allocation num-rules)
  (let ((cur-impossible (aref all-impossible r)))
    (when (and (not (gethash r allocation)) (= (hash-table-count cur-impossible) (- num-rules 1)))
      (iter (for pos from 0)
            (until (not (gethash pos cur-impossible)))
            (finally (progn
                       (setf (gethash r allocation) pos)
                       (iter (for other-impossible in-vector all-impossible)
                             (for other from 0)
                             (when (/= other r)
                               (setf (gethash pos other-impossible) t)
                               (check-impossible other all-impossible allocation num-rules)))))))))

(defun check-ticket (ticket rules impossible allocation)
  (iter (for r from 0 to (- (length rules) 1))
        (for rule in-vector rules)
        (when (not (gethash r allocation))
          (iter (for v in ticket)
                (for p from 0)
                (for cur-impossible = (aref impossible r))
                (match rule
                   ((guard (list _ min1 max1 min2 max2)
                           (and (or (< v min1) (> v max1))
                                (or (< v min2) (> v max2))))
                    (setf (gethash p cur-impossible) t)
                    (check-impossible r impossible allocation (length rules))))))))

(defun remove-invalid-tickets (tickets rules)
  (remove-if (lambda (ticket) (> (count-invalid-values ticket rules) 0)) tickets))

(defun my-ticket-value (lines)
  (bind (((my-ticket rules tickets) (read-rules-and-tickets lines)))
    (setf tickets (remove-invalid-tickets tickets rules))
    (iter (with impossible = (initialise-impossible rules))
          (with len = (length rules))
          (with allocation = (make-hash-table))
          (while (< (hash-table-count allocation) len))
          (iter (for ticket in tickets)
                (check-ticket ticket rules impossible allocation))
          (finally (return (iter (for rule in-vector rules)
                                 (for r from 0)
                                 (match rule
                                   ((list* 'departure _)
                                    (multiply (aref my-ticket (gethash r allocation)))))))))))

(defun part-1 () (count-all-invalid (read-lines)))
(defun part-2 () (my-ticket-value (read-lines)))
