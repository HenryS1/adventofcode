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
  (declare (optimize (speed 3)) (fixnum n))
  (if (= n 0)
      1
      (let ((len (number-length n)))
        
        (if (= (mod len 2) 0)
            (let ((shift (expt 10 (floor len 2))))
              (values (floor n shift) (mod n shift)))
            (the fixnum (* n 2024))))))

(declaim (inline apply-rules-with-counts))
(defun apply-rules-with-counts (stones new-stones)
  (declare (optimize (speed 3)))
  (clrhash new-stones)
  (loop for stone  being the hash-keys of stones using (hash-value count)
        do (multiple-value-bind (fst snd) (apply-rules stone)
             (setf (the fixnum (gethash fst new-stones))
                   (+ (the fixnum (gethash fst new-stones 0)) (the fixnum count)))
             (when snd 
               (setf (the fixnum (gethash snd new-stones))
                     (+ (the fixnum (gethash snd new-stones 0)) (the fixnum count))))))
  new-stones)

(declaim (inline next-stones))
(defun next-stones (current-stones one-stones other-stones)
  (declare (optimize (speed 3)))
  (if (eq current-stones one-stones)
      other-stones one-stones))

(defun part1 ()
  (let ((input (read-input "day11input"))
        (one-stones (make-hash-table))
        (other-stones (make-hash-table)))
    (loop for stone in input do (setf (gethash stone one-stones) 1))
    (loop for current-stones = one-stones then (next-stones current-stones one-stones other-stones)
          for next-stones = (next-stones current-stones one-stones other-stones)
          for i from 0 to 24
          do (apply-rules-with-counts current-stones next-stones)
          finally (return (loop for count being the hash-values of current-stones
                                summing count fixnum)))))

(defun part2 ()
  (let ((input (read-input "day11input"))
        (one-stones (make-hash-table :size 10000))
        (other-stones (make-hash-table :size 10000)))
    (loop for stone in input do (setf (gethash stone one-stones) 1))
    (loop for current-stones = one-stones then (next-stones current-stones one-stones other-stones)
          for next-stones = (next-stones current-stones one-stones other-stones)
          for i from 0 to 74
          do (apply-rules-with-counts current-stones next-stones)
          finally (return (loop for count being the hash-values of current-stones
                                summing count fixnum)))))

