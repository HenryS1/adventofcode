(defpackage :day8
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :metabang-bind)
  (:export 
   :calibration-value
   :total-calibration-value
   :digits-in-line
   :all-calibration-values))

(in-package :day8)

(defstruct antennas (rows 0 :type fixnum) (columns 0 :type fixnum) (locations "" :type string))

(defun read-input (file)
  (let* ((lines (with-open-file (f file)
                  (loop for line = (read-line f nil nil)
                        while line collect line)))
         (rows (length lines))
         (columns (length (car lines)))
         (locations (apply #'concatenate 'string lines)))
    (make-antennas :rows rows :columns columns :locations locations)))

(defun find-same-frequency-antennas (antennas)
  (let ((locations (antennas-locations antennas)))
    (loop with same-frequency-locations = (make-hash-table)
          for i from 0
          for c across locations
          when (char/= c #\.)
            do (push i (gethash c same-frequency-locations))
          finally (return same-frequency-locations))))

(defun find-antinode-locations (one other rows columns)
  (let* ((one-row (floor one columns))
         (other-row (floor other columns))
         (one-column  (mod one columns))
         (other-column (mod other columns))
         (row-difference (- one-row other-row))
         (col-difference (- one-column other-column))
         (row-from-one (+ one-row row-difference))
         (col-from-one (+ one-column col-difference))
         (row-from-other (- other-row row-difference))
         (col-from-other (- other-column col-difference))
         (one-side (when (and (<= 0 row-from-one (- rows 1))
                              (<= 0 col-from-one (- columns 1)))
                     (+ (* row-from-one columns) col-from-one)))
         (other-side (when (and (<= 0 row-from-other (- rows 1))
                                (<= 0 col-from-other (- columns 1)))
                       (+ (* row-from-other columns) col-from-other))))
    (values one-side other-side)))

(defun find-all-antinodes (same-frequency-locations rows columns antinodes)
  (loop for rest on same-frequency-locations
        for one = (car rest)
        while (cdr rest)
        do (loop for others on (cdr rest)
                 for other = (car others)
                 do (multiple-value-bind (one-side other-side) 
                        (find-antinode-locations one other rows columns)
                      (when one-side (setf (gethash one-side antinodes) t))
                      (when other-side (setf (gethash other-side antinodes) t))))))

(defun find-antinodes (antennas)
  (let ((rows (antennas-rows antennas))
        (columns (antennas-columns antennas))
        (same-frequency-locations (find-same-frequency-antennas antennas))
        (antinodes (make-hash-table)))
    (loop for antenna-group being the hash-values of same-frequency-locations
          do (find-all-antinodes antenna-group rows columns antinodes)
          finally (return antinodes))))

(defun part1 ()
  (let ((antennas (read-input "day8input")))
    (find-antinodes antennas)))

(defun find-aligned-antinode-locations (one other rows columns antinodes)
  (let* ((one-row (floor one columns))
         (other-row (floor other columns))
         (one-column  (mod one columns))
         (other-column (mod other columns))
         (base-row-difference (- one-row other-row))
         (base-col-difference (- one-column other-column))
         (divisor (gcd base-row-difference base-col-difference))
         (row-difference (floor base-row-difference divisor))
         (col-difference (floor base-col-difference divisor)))
    (loop for row = one-row then (+ row row-difference)
          for column = one-column then (+ column col-difference)
          while (and (<= 0 row (- rows 1))
                     (<= 0 column (- columns 1)))
          do (setf (gethash (+ (* row columns) column) antinodes) t))
    (loop for row = one-row then (- row row-difference)
          for column = one-column then (- column col-difference)
          while (and (<= 0 row (- rows 1))
                     (<= 0 column (- columns 1)))
          do (setf (gethash (+ (* row columns) column) antinodes) t))))

(defun find-all-aligned-antinodes (same-frequency-locations rows columns antinodes)
  (loop for rest on same-frequency-locations
        for one = (car rest)
        while (cdr rest)
        do (loop for others on (cdr rest)
                 for other = (car others)
                 do (find-aligned-antinode-locations one other rows columns antinodes))))

(defun find-aligned-antinodes (antennas)
  (let ((rows (antennas-rows antennas))
        (columns (antennas-columns antennas))
        (same-frequency-locations (find-same-frequency-antennas antennas))
        (antinodes (make-hash-table)))
    (loop for antenna-group being the hash-values of same-frequency-locations
          do (find-all-aligned-antinodes antenna-group rows columns antinodes)
          finally (return antinodes))))

(defun part2 ()
  (let ((antennas (read-input "day8input")))
    (find-aligned-antinodes antennas)))
