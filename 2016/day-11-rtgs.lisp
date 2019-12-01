(load "../2018/queue.lisp")
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
              (when (and (or e1 e2)
)
                (in outer (collect (remove-if #'null (list e1 e2))))))))

(defun safe (eqpmnt)
  (or (null eqpmnt)
      (every (lambda (e) (equal (car e) (caar eqpmnt))) eqpmnt)
      (every (lambda (e) (or (not (equal (car e) 'm))
                             (find (cons 'g (cdr e)) eqpmnt :test 'equal))) eqpmnt)))

(defun eqpmnt< (one other)
  (or (string< (symbol-name (car one)) (symbol-name (car other)))
      (equal (car one) (car other))
      (string< (symbol-name (cdr one)) (symbol-name (cdr other)))))

(defun move (state m dir)
  (destructuring-bind (st e) state
    (when (and (>= (+ e dir) 0)
               (< (+ e dir) 4))
      (let* ((remaining (sort (copy-seq (set-difference (nth e st) m :test 'equal)) #'eqpmnt<))
             (added (sort (copy-seq (append m (nth (+ e dir) st))) #'eqpmnt<)))
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
                (+ e dir)))))))

(defun next-states (state)
  (destructuring-bind (st e) state
    (iter (for m in (choose-one-or-two (nth e st)))
          (awhen (move state m 1)
            (collect it))
          (awhen (move state m -1)
            (collect it)))))

(defun make-moves (start)
  (let ((q (make-queue start))
        (seen (make-hash-table :test 'equal)))
    (iter (for current = (poll q))
          (while current)
          (for (st e) = current)
;          (format t "ST ~a E ~a~%" st e)
          (until (every (lambda (l) (null l)) (butlast st)))
;          (format t "CURRENT ~a~%" current)
          (iter (for next in (next-states current))
                (when (not (gethash next seen))
                  (setf (gethash next seen) t)
                  (enqueue next q)))
          (finally (return current)))))
