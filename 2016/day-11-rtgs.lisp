(load "../2018/priority-queue.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)

(defpackage :day11
  (:use :cl :cl-ppcre :iterate :anaphora))

(in-package :day11)

(defun generators (line) 
  (mapcar (lambda (s) (cons 'g (intern (string-upcase (car (split "\\s+" s))))))
          (all-matches-as-strings "\\w+ generator" line)))

(defun microchips (line)
  (mapcar (lambda (s) (cons 'm (intern (string-upcase (car (split "(\\s|-)+" s))))))
          (all-matches-as-strings "\\w+-\\w+ microchip" line)))

(defun read-state ()
  (iter (for line in-file "day11.txt" using #'read-line)
        (for gs = (generators line))
        (for ms = (microchips line))
        (collect (append gs ms))))

(defun elevator-safe (e1 e2)
  (or (null e1)
      (equal (car e1) (car e2))
      (equal (cdr e1) (cdr e2))))

(defun choose-one-or-two (eqpmnt)
  (iter outer (for e1s first (cons nil eqpmnt) then (cdr e1s))
        (while (cdr e1s))
        (for e1 = (car e1s))
        (iter (for e2 in (cdr e1s))
              (when (and (or e1 e2))
                (in outer (collect (remove-if #'null (list e1 e2))))))))

(defun safe (eqpmnt)
  (or (null eqpmnt)
      (every (lambda (e) (equal (car e) (caar eqpmnt))) eqpmnt)
      (every (lambda (e) (or (not (equal (car e) 'm))
                             (find (cons 'g (cdr e)) eqpmnt :test 'equal))) eqpmnt)))

(defun eqpmnt< (one other)
  (or (string< (symbol-name (car one)) (symbol-name (car other)))
      (and (equal (car one) (car other))
           (string< (symbol-name (cdr one)) (symbol-name (cdr other))))))

(defun move (state m dir)
  (destructuring-bind (st e cnt) state
    (when (and (>= (+ e dir) 0)
               (< (+ e dir) 4))
      (let* ((remaining (sort (copy-seq (set-difference (nth e st) m :test 'equal)) #'eqpmnt<))
             (added (sort (copy-seq (append m (nth (+ e dir) st))) #'eqpmnt<)))
        ;; (format t "remaining ~a~%" remaining)
        ;; (format t "added ~a~%" added)
        (when (and (safe remaining) 
                   (safe added))
          (list (iter (for fl from 0 to 3)
                      (when (and (/= e fl)
                                 (/= (+ e dir) fl))
                        (collect (nth fl st)))
                      (when (= e fl)
                        (collect remaining))
                      (when (= (+ e dir) fl)
                        (collect added)))
                (+ e dir)
                (+ cnt 1)))))))

(defun next-states (state)
  (destructuring-bind (st e cnt) state
    (declare (ignore cnt))
    (iter (for m in (choose-one-or-two (nth e st)))
          (awhen (move state m 1)
            (collect it))
          (awhen (move state m -1) 
            (collect it)))))

(defparameter *test-start* 
  '(((M . L) (M . H)) ((G . H)) ((G . L)) nil))


(defun relabel-state (st)
  (let ((table (make-hash-table :test 'equal)))
    (iter (with counter = 0)
          (for fl in st)
          (collect (iter (for entry in fl)
                         (when (not (gethash (cdr entry) table))
                           (setf (gethash (cdr entry) table) counter)
                           (incf counter))
                         (collect (cons (car entry) (gethash (cdr entry) table))))))))

(defun priority (state)
  (destructuring-bind ((f1 f2 f3 f4) e cnt) state
    (declare (ignore e f4 cnt))
    (+ cnt
     (* 3 (length f1))
     (* 2 (length f2))
     (length f3))))

(defun state< (one other)
  (< (priority one) (priority other)))

(defun make-moves (start)
  (let ((q (make-pq #'state<))
        (seen (make-hash-table :test 'equal)))
    (insert-pq start q)
    (iter (for current = (pop-pq q))
          (while current)
;          (format t "CURRENT ~a~%" current)
          (for (st e cnt) = current)
          (until (every (lambda (l) (null l)) (butlast st)))
          (iter (for next in (next-states current))
                ;(format t "NEXT ~a~%" next)
                ;(setf (car next) (relabel-state (car next)))
                (when (not (gethash (butlast next) seen))
                  (setf (gethash (butlast next) seen) t)
                  (insert-pq next q)))
          (finally (return current)))))

(defun answer-1 ()
  (make-moves (list (read-state) 0 0)))

