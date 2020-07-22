(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :iterate)
  (ql:quickload :metabang-bind))

(defpackage :day18
  (:use :cl :iterate :bind))

(in-package :day18)

(defun read-input ()
  (let* ((lines (iter (for line in-file "input18" using #'read-line) (collect line)))
         (rows (length lines))
         (cols (length (car lines)))
         (grid (make-array (* rows cols) :element-type 'bit :initial-element 0)))
    (iter (for line in lines)
          (for row from 0)
          (iter (for c in-string line)
                (for col from 0)
                (when (char= c #\#)
                  (setf (aref grid (+ (* cols row) col)) 1))))
    (values rows cols grid)))

(defun on-neighbours (rows cols row col grid)
  (let ((count 0))
    (iter (for r from (max 0 (- row 1)) to (min (+ row 1) (- rows 1)))
          (iter (for c from (max 0 (- col 1)) to (min (+ col 1) (- cols 1)))
                (when (or (/= r row) (/= c col))
                  (incf count (aref grid (+ (* r cols) c))))))
    count))

(defun tick (rows cols grid)
  (iter (with new-grid = (make-array (* rows cols) :element-type 'bit :initial-element 0))
        (for row from 0 to (- rows 1))
        (iter (for col from 0 to (- cols 1))
              (for neighbour-count = (on-neighbours rows cols row col grid))
              (when (or (and (= (aref grid (+ (* row cols) col)) 1) (= neighbour-count 2))
                        (= neighbour-count 3))
                (setf (aref new-grid (+ (* row cols) col)) 1)))
        (finally (return new-grid))))

(defun evolve (rows cols initial-grid iterations)
  (iter (for i from 0 to iterations)
        (for grid first initial-grid then (tick rows cols grid))
        (finally (return grid))))

(defun part-one ()
  (bind (((:values rows cols grid) (read-input)))
    (reduce #'+ (evolve rows cols grid 100))))

(defun evolve-with-corners-on (rows cols initial-grid iterations)
  (iter (for i from 0 to iterations)
        (for grid first initial-grid then (tick rows cols grid))
        (setf (aref grid 0) 1
              (aref grid (- cols 1)) 1
              (aref grid (* (- rows 1) cols)) 1
              (aref grid (+ (* (- rows 1) cols) (- cols 1))) 1)
        (finally (return grid))))

(defun part-two ()
  (bind (((:values rows cols grid) (read-input)))
    (reduce #'+ (evolve-with-corners-on rows cols grid 100))))
