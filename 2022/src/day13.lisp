(defpackage :day13
  (:use 
   :cl 
   :aoc.functional
   :cl-ppcre 
   :trivia 
   :trivia.ppcre 
   :iterate 
   :alexandria 
   :anaphora 
   :metabang-bind
   :aoc.datastructures))

(in-package :day13)

(defun read-list (line)
  (let (*read-eval*)
    (with-input-from-string (s (regex-replace-all 
                                "," (regex-replace-all
                                     "\\]" (regex-replace-all "\\[" line "(") ")") " "))
      (read s))))

(defun read-lines ()
  (iter (for line in-file "day13.input" using #'read-line)
    (if (> (length line) 0)
        (collect (read-list line))
        (collect 'empty))))

(defun group-lines (lines)
  (when lines
    (bind (((group . rest) (take-drop-while lines (lambda (l) (not (equal l 'empty))))))
      (cons group (group-lines (cdr rest))))))

(defun compare-packets (p1 p2)
  (cond ((and (numberp p1) (numberp p2)) 
         (cond ((< p1 p2) 'lt)
               ((> p1 p2) 'gt)
               (t 'e)))
        ((and (consp p1) (consp p2))
         (case (compare-packets (car p1) (car p2))
           (lt 'lt)
           (gt 'gt)
           (e (compare-packets (cdr p1) (cdr p2)))))
        ((and (numberp p1) (consp p2)) 
         (compare-packets (list p1) p2))
        ((and (consp p1) (numberp p2))
         (compare-packets p1 (list p2)))
        ((and (null p1) (null p2) 'e))
        ((null p1) 'lt)
        ((null p2) 'gt)
        (t (error "unexpected"))))

(defun correct-order-indices (groups)
  (iter (for (one other) in groups)
    (for i from 1)
    (for comp = (compare-packets one other))
    (when (or (equal comp 'lt) (equal comp 'e))
      (collect i))))

(defun part1 ()
  (reduce #'+ (correct-order-indices (group-lines (read-lines)))))

(defun find-all-packets ()
  (remove-if (lambda (l) (equal l 'empty)) (read-lines)))

(defun find-divider-packets (packets)
  (iter (for p in packets)
    (for i from 1)
    (when (or (equal p '((2))) (equal p '((6))))
      (collect i))))

(defun part2 ()
  (reduce #'* (find-divider-packets 
    (sort (append '(((2)) ((6))) (find-all-packets))
          (lambda (a b) (equal (compare-packets a b) 'lt))))))
