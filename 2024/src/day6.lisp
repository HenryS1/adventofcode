(defpackage :day6
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

(in-package :day6)

(defstruct route (rows 0 :type fixnum) (columns 0 :type fixnum) (locations "" :type string))

(defun read-input (file)
  (let* ((lines (with-open-file (f file)
                  (loop for line = (read-line f nil nil)
                        while line collect line)))
         (rows (length lines))
         (columns (length (car lines)))
         (locations (apply #'concatenate 'string lines)))
    (make-route :rows rows :columns columns :locations locations)))

(defun find-start-position (route)
  (position #\^ (route-locations route)))

(declaim (inline change-direction))
(defun change-direction (direction)
  (case direction
    (up 'right)
    (right 'down)
    (down 'left)
    (left 'up)))

(declaim (inline next-index))
(defun next-index (current direction columns)
  (case direction 
    (up (- current columns))
    (down (+ current columns))
    (right (+ current 1))
    (left (- current 1))))

(declaim (inline would-exit))
(defun would-exit (current direction rows columns)
  (case direction
    (up (= (floor current columns) 0))
    (down (= (floor current columns) (- rows 1)))
    (right (= (mod current columns) (- columns 1)))
    (left (= (mod current columns) 0))))

(defun print-route (route)
  (let ((columns (route-columns route))
        (locations (route-locations route)))
    (loop for index from 0 
          for c across locations
          do (format t "~a" c)
          when (= (mod index columns) (- columns 1))
            do (format t "~%"))))

(defun track-route (route)
  (let* ((visited (make-hash-table))
         (start (find-start-position route))
         (rows (route-rows route))
         (columns (route-columns route))
         (locations (route-locations route)))
    (setf (gethash start visited) t)
    (loop with index = start 
          with direction = 'up
          while (not (would-exit index direction rows columns))
          for next-index = (next-index index direction columns)
          for next-location = (aref locations next-index)
          if (char= next-location #\#)
            do (setf direction (change-direction direction))
          else
            do (setf index next-index)
               (setf (gethash index visited) t)
          finally (return visited))))

(defun part1 ()
  (let ((route (read-input "day6input")))
    (track-route route)))

(declaim (inline direction-num))
(defun direction-num (direction)
  (case direction
    (up 0)
    (right 1)
    (down 2)
    (left 3)))

(declaim (inline detect-loop))
(defun detect-loop (start start-direction route visited obstruction)
  (declare (optimize (speed 3)))
  (let* ((rows (route-rows route))
         (columns (route-columns route))
         (locations (route-locations route)))
    (declare ((simple-array fixnum) visited)
             ((unsigned-byte 32) obstruction columns rows)
             ((simple-array character) locations))
    (loop with index of-type (unsigned-byte 32) = start
          with direction = start-direction
          while (not (would-exit index direction rows columns))
          for next-index of-type (unsigned-byte 32) = (next-index index direction columns)
          for next-location = (aref locations next-index)
          for direction-num = (direction-num direction)
          when (= (aref visited (+ (ash index 2) direction-num)) obstruction)
            do (return t)
          do (setf (aref visited (+ (ash index 2) direction-num)) obstruction)
          if (char= next-location #\#)
            do (setf direction (change-direction direction))
          else 
            do (setf index next-index))))

(defun find-loops (route)
  (let* ((start (find-start-position route))
         (rows (route-rows route))
         (columns (route-columns route))
         (locations (route-locations route))
         (visited-in-loop (make-array (* (length locations) 4) :element-type 'fixnum))
         (visited (make-array (length locations) :element-type 'bit))
         (obstructions (make-array (length locations) :element-type 'bit)))
    (declare (optimize (speed 3))
             ((simple-array character) locations))
    (loop with index fixnum = start
          with direction = 'up
          while (not (would-exit index direction rows columns))
          for next-index fixnum = (next-location index direction columns)
          for next-location character = (aref locations next-index)
          if (char= next-location #\#)
            do (setf direction (change-direction direction))
          else 
            do (when (/= (aref visited next-index) 1)
                 (setf (aref locations next-index) #\#)
                 (when (detect-loop index direction route visited-in-loop next-index)
                   (setf (aref obstructions next-index) 1))
                 (setf (aref locations next-index) next-location))
               (setf index next-index)
          do (setf (aref visited index) 1)
          finally (progn 
                    (setf (aref obstructions start) 0)
                    (return (reduce #'+ obstructions))))))

(defun part2 ()
  (let ((route (read-input "day6input")))
    (find-loops route)))
