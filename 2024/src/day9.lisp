(defpackage :day9
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

(in-package :day9)

(defun read-input (file)
  (let* ((line (car (with-open-file (f file)
                      (loop for line = (read-line f nil nil)
                            while line collect line))))
         (digits (make-array (length line) :element-type '(unsigned-byte 8))))
    (loop for d across line
          for i from 0
          do (setf (aref digits i) (digit-char-p d))
          finally (return digits))))

(defun total-blocks-to-allocate (blocks)
  (loop for b across blocks
        for i from 0
        when (= (mod i 2) 0)
          sum b))

(defun allocate-blocks (blocks)
  (let ((space-for-allocation (make-array (total-blocks-to-allocate blocks) :element-type '(unsigned-byte 16))))
    (loop with fill-pointer = 0
          with back-pointer = (- (length blocks) 1)
          for block across blocks
          for block-index from 0
          for block-id = (floor block-index 2)
          while (< fill-pointer (length space-for-allocation))
          if (= (mod block-index 2) 0)
            do (loop for i from 1 to block
                     do (setf (aref space-for-allocation fill-pointer) block-id)
                        (decf (aref blocks block-index))
                        (incf fill-pointer))
          else 
            do (loop for i from 1 to block
                     for back-block-id = (floor back-pointer 2)
                     for remaining = (decf (aref blocks back-pointer))
                     do (setf (aref space-for-allocation fill-pointer) back-block-id)
                        (incf fill-pointer)
                        (decf (aref blocks block-index))
                     when (= remaining 0)
                       do (decf back-pointer 2))
          finally (return space-for-allocation))))

(defun part1 ()
  (let* ((blocks (read-input "day9input"))
         (allocation (allocate-blocks blocks)))
    (loop for i from 0 
          for block across allocation
          summing (* i block))))

(defun prefix-sum (blocks)
  (let ((result (make-array (length blocks) :element-type 'fixnum)))
    (loop for total = 0 then (+ total block)
          for block across blocks
          for i from 0 to (- (length blocks) 1)
          do (setf (aref result i) total)
          finally (return result))))

(defun allocate-blocks-contiguously (blocks)
  (declare ((simple-array (unsigned-byte 8)) blocks) (optimize (speed 3)))
  (let ((file-allocations (make-array (+ (floor (length blocks) 2) 1)
                                      :element-type '(unsigned-byte 16)))
        (indices-before (prefix-sum blocks)))
    (loop with back-pointer = (- (length blocks) 1)
          while (> back-pointer 0)
          do (loop with moved = nil
                   for fill-pointer = 1 then (+ fill-pointer 2)
                   while (< fill-pointer back-pointer)
                   when (>= (aref blocks fill-pointer) (aref blocks back-pointer))
                     do (decf (aref blocks fill-pointer)
                              (aref blocks back-pointer))
                        (setf (aref file-allocations (floor back-pointer 2))
                              (aref indices-before fill-pointer))
                        (incf (aref indices-before fill-pointer)
                              (aref blocks back-pointer))
                        (setf moved t)
                        (decf back-pointer 2)
                        (return nil)
                   finally (progn (when (not moved)
                                    (setf (aref file-allocations (floor back-pointer 2)) 
                                          (aref indices-before back-pointer)))
                                  (decf back-pointer 2)))
          finally (return file-allocations))))

(defun file-allocation-checksum (blocks allocations)
  (loop for file-number from 0 
        for start across allocations
        for file-size = (aref blocks (* file-number 2))
        summing (loop for i from 0 to (- file-size 1)
                      summing (* (+ start i) file-number))))

(defun part2 ()
  (let* ((blocks (read-input "day9input"))
         (allocations (allocate-blocks-contiguously blocks)))
    (file-allocation-checksum blocks allocations)))
