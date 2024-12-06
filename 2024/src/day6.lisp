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

(defun change-direction (direction)
  (case direction
    (up 'right)
    (right 'down)
    (down 'left)
    (left 'up)))

(defun next-index (current direction columns)
  (case direction 
    (up (- current columns))
    (down (+ current columns))
    (right (+ current 1))
    (left (- current 1))))

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
;          do (format t "~a~%" (list direction (floor index columns) (mod index columns) (aref locations index)))
          if (char= next-location #\#)
            do (setf direction (change-direction direction))
          else
            do (setf index next-index)
               (setf (gethash index visited) t)
          finally (return visited))))

(defun part1 ()
  (let ((route (read-input "day6input")))
    (track-route route)))

(defun detect-loop (start start-direction route)
  (let* ((visited (make-hash-table :test 'equal))
         (rows (route-rows route))
         (columns (route-columns route))
         (locations (route-locations route)))
    (loop with index = start
          with direction = start-direction
          while (not (would-exit index direction rows columns))
          for next-index = (next-index index direction columns)
          for next-location = (aref locations next-index)
          when (gethash (cons index direction) visited)
            do (return t)
          do (setf (gethash (cons index direction) visited) t)
          if (char= next-location #\#)
            do (setf direction (change-direction direction))
          else 
            do (setf index next-index))))

(defun cons-less (one other)
  (or (< (car one) (car other))
      (and (= (car one) (car other))
           (< (cdr one) (cdr other)))))

(defun find-loops (route)
  (let* ((visited (make-hash-table))
         (start (find-start-position route))
         (rows (route-rows route))
         (columns (route-columns route))
         (locations (route-locations route))
         (obstructions (make-hash-table)))
    (loop with index = start
          with direction = 'up
          while (not (would-exit index direction rows columns))
          for next-index = (next-location index direction columns)
          for next-location = (aref locations next-index)
          if (char= next-location #\#)
            do (setf direction (change-direction direction))
          else 
            do (when (not (gethash next-index visited))
                 (setf (aref locations next-index) #\#)
                 (when (detect-loop index direction route)
                   (setf (gethash next-index obstructions) t))
                 (setf (aref locations next-index) next-location))
               (setf index next-index)
          do (setf (gethash index visited) t)
          finally (progn 
                    (remhash start obstructions)
                    (return obstructions)))))

(defun part2 ()
  (let ((route (read-input "day6input")))
    (find-loops route)))
