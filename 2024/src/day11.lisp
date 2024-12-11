(defpackage :day11
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

(in-package :day11)

(defun ints (line) 
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" line)))

(defun read-input (file)
  (with-open-file (f file)
    (ints (read-line f nil nil))))

(declaim (inline number-length))
(defun number-length (n)
  (declare (optimize (speed 3)) (fixnum n))
  (loop for len fixnum from 1
        for remaining fixnum = (floor n 10) then (floor remaining 10)
        while (> remaining 0)
        finally (return len)))

(declaim (inline apply-rules))
(defun apply-rules (n)
  (declare (optimize (speed 3))
           (fixnum n))
  (if (= n 0)
      1
      (let ((len (number-length n)))
        
        (if (= (mod len 2) 0)
            (let ((shift (expt 10 (floor len 2))))
              (values (floor n shift) (mod n shift)))
            (the fixnum (* n 2024))))))

(declaim (inline apply-rules-with-counts))
(defun apply-rules-with-counts (stones)
  (declare (optimize (speed 3)))
  (let ((new-stones (make-hash-table :size (hash-table-size stones))))
    (loop for stone  being the hash-keys of stones using (hash-value count)
          do (multiple-value-bind (fst snd) (apply-rules stone)
               (setf (the fixnum (gethash fst new-stones))
                     (+ (the fixnum (gethash fst new-stones 0)) (the fixnum count)))
               (when snd 
                 (setf (the fixnum (gethash snd new-stones))
                       (+ (the fixnum (gethash snd new-stones 0)) (the fixnum count))))))
    new-stones))

(defun part1 ()
  (let ((input (read-input "day11input"))
        (stones (make-hash-table)))
    (loop for stone in input do (setf (gethash stone stones) 1))
    (loop for current-stones = stones then (apply-rules-with-counts current-stones)
          for i from 0 to 24
          finally (return (loop for count being the hash-values of current-stones
                                summing count)))))

(defun part2 ()
  (let ((input (read-input "day11input"))
        (stones (make-hash-table)))
    (loop for stone in input do (setf (gethash stone stones) 1))
    (loop for current-stones = stones then (apply-rules-with-counts current-stones)
          for i from 0 to 74
          finally (return (loop for count fixnum being the hash-values of current-stones
                                summing count fixnum)))))
