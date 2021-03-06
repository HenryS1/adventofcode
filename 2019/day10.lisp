(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day10
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day10)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-asteroids ()
  (iter outer
        (for line in-file "input10" using #'read-line)
        (for r from 0)
        (iter (for c from 0 to (- (length line) 1))
              (for ch in-string line)
              (when (char= ch #\#)
                (in outer (collect (cons c r)))))))

(defun find-slope (one other)
  (let* ((y-diff (- (cdr one) (cdr other)))
         (x-diff (- (car other) (car one)))
         (d (gcd x-diff y-diff)))
    (cons (/ x-diff d) (/ y-diff (abs d)))))

(defun negate (slope)
  (cons (- (car slope)) (- (cdr slope))))

(defun quadrant (one)
  (destructuring-bind (x . y) one
      (cond ((and (>= x 0) (>= y 0)) 1)
            ((and (> x 0) (< y 0)) 2)
            ((and (<= x 0) (< y 0)) 3)
            ((and (< x 0) (>= y 0)) 4))))

(defun comp-slope (one other q)
  (cond ((= (cdr other) 0) (= q 1))
        ((= (car other) 0) (= q 2))
        ((= (cdr one) 0) (= q 4))
        ((= (car one) 0) (or (= q 1) (= q 3)))
        (t (< (/ (car one) (cdr one)) (/ (car other) (cdr other))))))

(defun comp-orientation (one other)
  (let ((one-q (quadrant (car one)))
        (other-q (quadrant (car other))))
    (or (< one-q other-q)
        (and (= one-q other-q)
             (comp-slope (car one) (car other) one-q)))))

(defun comp-distance (one other)
  (< (car one) (car other)))

(defun find-visible-from (asteroids)
  (iter (with visible = (make-hash-table :test 'equal))
        (for rest first asteroids then (cdr rest))
        (while (cdr rest))
        (for one = (car rest))
        (iter (for other in (cdr rest))
              (when (not (equal one other))
                (let* ((distance (+ (expt (- (car one) (car other)) 2)
                                    (expt (- (cdr one) (cdr other)) 2)))
                       (slope (find-slope one other))
                       (key-one (cons one slope))
                       (key-other (cons other (negate slope))))
                  (push (cons distance other) (gethash key-one visible))
                  (push (cons distance one) (gethash key-other visible)))))
        (finally (return (progn (iter (for (k v) in-hashtable visible)
                                      (setf (gethash k visible) 
                                            (mapcar #'cdr (sort v #'comp-distance))))
                                visible)))))

(defun find-most-visible (visible)
  (iter (with visible-neighbours = (make-hash-table :test 'equal))
        (for ((one . slope) distance) in-hashtable visible)
        (if (not (gethash one visible-neighbours))
            (setf (gethash one visible-neighbours) 1)
            (incf (gethash one visible-neighbours)))
        (finally (return (iter (with best-n = 0)
                               (with best-coord = nil)
                               (for (one n) in-hashtable visible-neighbours)
                               (when (> n best-n)
                                 (setf best-n n
                                       best-coord one))
                               (finally (return (cons best-coord best-n))))))))

(defun answer-1 ()
  (find-most-visible (find-visible-from (read-asteroids))))

(defun collect-visible (asteroids)
  (let* ((visible-from (find-visible-from asteroids))
         (most-visible (car (find-most-visible visible-from))))
    (iter (for ((a . slp) vis) in-hashtable visible-from)
          (when (equal a most-visible)
            (collect (cons slp vis))))))

(defun destroy-asteroids (asteroids)
  (setq *print-circle* t)
  (iter (with ordered = (sort (collect-visible asteroids) #'comp-orientation))
        (with cnt = 0)
        (with destroyed = (list))
        (while (< cnt 200))
        (for sn in (apply #'circular-list ordered))
        (when (cdr sn)
          (push (pop (cdr sn)) destroyed)
          (incf cnt))
        (finally (return destroyed))))

