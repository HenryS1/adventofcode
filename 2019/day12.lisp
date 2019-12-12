(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day12
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day12)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-planets ()
  (iter (for line in-file "input12" using #'read-line)
        (collect (list (ints line) (list 0 0 0)))))

(defun adjust-velocities (planets)
  (iter outer
        (for (p1 v1) in planets)
        (iter (for (p2 v2) in planets)
              (when (not (equal p1 p2))
                (incf (car v1) (signum (- (car p2) (car p1))))
                (incf (cadr v1) (signum (- (cadr p2) (cadr p1))))
                (incf (caddr v1) (signum (- (caddr p2) (caddr p1))))))
        (collect (list p1 v1))))

(defun adjust-positions (planets)
  (iter outer 
        (for (p1 v1) in planets)
        (collect (list (mapcar #'+ p1 v1) v1))))

(defun tick (planets)
  (adjust-positions (adjust-velocities planets)))

(defun total-energy (planet)
  (destructuring-bind ((p1 p2 p3) (v1 v2 v3)) planet
    (* (+ (abs p1) (abs p2) (abs p3))
       (+ (abs v1) (abs v2) (abs v3)))))

(defun answer-1 ()
  (iter (for i from 0 to 1000)
        (for planets first (read-planets) then (tick planets))
        (finally (return (* (reduce #'+ (mapcar #'total-energy planets)))))))

(defun positions (planets)
  (mapcar #'first planets))

(defun period-finder ()
  (let ((seen (make-hash-table :test 'equal)))
    (lambda (next i)
      (if (< (length (gethash next seen)) 2)
          (progn (push i (gethash next seen)) nil)
          (gethash next seen)))))

(defun coords (planets accessor)
  (mapcar (lambda (p) (list (funcall accessor (car p)) (funcall accessor (cadr p))))
          planets))

(defun xs (planets)
  (coords planets #'first))

(defun ys (planets)
  (coords planets #'second))

(defun zs (planets)
  (coords planets #'third))

(defun find-period (planets coord)
  (iter (with seen = (make-hash-table :test 'equal))
        (for ps first planets then (tick ps))
        (for i from 0)
        (for cs = (funcall coord ps))
        (while (not (gethash cs seen)))
        (setf (gethash cs seen) t)
        (finally (return i))))

(defun answer-2 ()
  (lcm (find-period (read-planets) #'xs)
       (find-period (read-planets) #'ys)
       (find-period (read-planets) #'zs)))
