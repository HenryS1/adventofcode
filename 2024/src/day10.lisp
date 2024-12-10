(defpackage :day10
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

(in-package :day10)

(defstruct topography (rows 0 :type fixnum) (columns 0 :type fixnum) 
           (locations (make-array 1 :element-type '(unsigned-byte 8)) 
            :type (simple-array (unsigned-byte 8))))

(defun read-input (file)
  (let* ((lines (with-open-file (f file)
                  (loop for line = (read-line f nil nil)
                        while line collect line)))
         (rows (length lines))
         (columns (length (car lines)))
         (locations (make-array (* rows columns) :element-type '(unsigned-byte 8))))
    (loop for line in lines
          for row from 0
          do (loop for d across line
                   for column from 0
                   do (setf (aref locations (+ (* row columns) column)) 
                            (digit-char-p d))))
    (make-topography :rows rows :columns columns :locations locations)))

(defun find-trail-heads (topography)
  (loop for i from 0
        for d across (topography-locations topography)
        when (= d 0)
          collect i))

(defun trail-score (start topography)
  (let ((rows (topography-rows topography))
        (columns (topography-columns topography))
        (locations (topography-locations topography))
        (seen-ends (make-hash-table)))
    (loop with trails = 0
          with stack = (list start)
          while stack
          for current = (pop stack)
          for current-value = (aref locations current)
          for row = (floor current rows)
          for column = (mod current columns)
          if (and (= current-value 9)
                  (not (gethash current seen-ends)))
            do (setf (gethash current seen-ends) t)
               (incf trails)
          else do (when (and (> row 0)
                             (= (aref locations (- current columns)) 
                                (+ current-value 1)))
                    (push (- current columns) stack))
                  (when (and (< row (- rows 1))
                             (= (aref locations (+ current columns))
                                (+ current-value 1)))
                    (push (+ current columns) stack))
                  (when (and (> column 0)
                             (= (aref locations (- current 1))
                                (+ current-value 1)))
                    (push (- current 1) stack))
                  (when (and (< column (- columns 1))
                             (= (aref locations (+ current 1))
                                (+ current-value 1)))
                    (push (+ current 1) stack))
          finally (return trails))))

(defun find-trail-scores (topography)
  (mapcar (lambda (trail-head) (trail-score trail-head topography)) (find-trail-heads topography)))

(defun part1 ()
  (let ((topography (read-input "day10input")))
    (reduce #'+ (find-trail-scores topography))))

(defun multiple-trail-score (start topography)
  (let ((rows (topography-rows topography))
        (columns (topography-columns topography))
        (locations (topography-locations topography)))
    (loop with trails = 0
          with stack = (list start)
          while stack
          for current = (pop stack)
          for current-value = (aref locations current)
          for row = (floor current rows)
          for column = (mod current columns)
          if (= current-value 9)
            do (incf trails)
          else do (when (and (> row 0)
                             (= (aref locations (- current columns)) 
                                (+ current-value 1)))
                    (push (- current columns) stack))
                  (when (and (< row (- rows 1))
                             (= (aref locations (+ current columns))
                                (+ current-value 1)))
                    (push (+ current columns) stack))
                  (when (and (> column 0)
                             (= (aref locations (- current 1))
                                (+ current-value 1)))
                    (push (- current 1) stack))
                  (when (and (< column (- columns 1))
                             (= (aref locations (+ current 1))
                                (+ current-value 1)))
                    (push (+ current 1) stack))
          finally (return trails))))

(defun find-multiple-trail-scores (topography)
  (mapcar (lambda (trail-head) (multiple-trail-score trail-head topography))
          (find-trail-heads topography)))

(defun part2 ()
  (let ((topography (read-input "day10input")))
    (reduce #'+ (find-multiple-trail-scores topography))))
