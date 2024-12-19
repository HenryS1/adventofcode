(defpackage :day15
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :queue
   :anaphora 
   :metabang-bind)
  (:export 
   :calibration-value
   :total-calibration-value
   :digits-in-line
   :all-calibration-values))

(in-package :day15)

(defstruct warehouse (rows 0 :type fixnum) (columns 0 :type fixnum) (locations "" :type string))

(defun read-warehouse (stream)
  (let* ((lines (loop for line = (read-line stream nil nil)
                      while (and line (> (length line) 0))
                      collect line))
         (rows (length lines))
         (columns (length (car lines)))
         (locations (apply #'concatenate 'string lines)))
    (make-warehouse :rows rows :columns columns :locations locations)))

(defun read-directions (stream)
  (let ((lines (loop for line = (read-line stream nil nil)
                     while line collect line)))
    (apply #'concatenate 'string lines)))

(defun read-input (file)
  (with-open-file (f file)
    (let ((warehouse (read-warehouse f)))
      (cons warehouse (read-directions f)))))

(defun find-start (warehouse)
  (loop for c across (warehouse-locations warehouse)
        for i fixnum from 0
        when (char= c #\@)
          do (return i)))

(declaim (inline next-available-square))
(defun find-next-available-square (locations current offset)
  (loop for index = (+ current offset) then (+ index offset)
        for c = (aref locations index)
        if (char= c #\#)
          do (return nil)
        else if (char= c #\.)
               do (return index)))

(defun print-locations (locations columns)
  (loop for i from 0 
        for c across locations
        for column = (mod i columns)
        do (format t "~a" c)
        when (= column (- columns 1))
          do (format t "~%")))

(declaim (inline gps-coordinate))
(defun gps-coordinate (index columns)
  (declare (optimize (speed 3)) (fixnum index columns))
  (let ((row (floor index columns))
        (column (mod index columns)))
    (+ (the fixnum (* 100 row)) column)))

(declaim (inline direction-offset))
(defun direction-offset (direction columns)
  (case direction
    (#\^ (- columns))
    (#\v columns)
    (#\> 1)
    (#\< -1)))

(defun make-moves (warehouse directions)
  (let ((columns (warehouse-columns warehouse))
        (locations (warehouse-locations warehouse)))
    (loop with current = (find-start warehouse)
          for direction across directions
          for offset = (direction-offset direction columns)
          for adjacent = (aref locations (+ current offset))
          do (cond ((char= adjacent #\.)
                    (setf (aref locations (+ current offset)) #\@)
                    (setf (aref locations current) #\.)
                    (incf current offset))
                   ((char= adjacent #\#))
                   ((char= adjacent #\O)
                    (let ((next-available (find-next-available-square locations current offset)))
                      (when next-available 
                        (setf (aref locations next-available) #\O)
                        (setf (aref locations current) #\.)
                        (setf (aref locations (+ current offset))  #\@)
                        (incf current offset))))))
    (loop for c across locations
          for index from 0
          when (char= c #\O)
            summing (gps-coordinate index columns))))

(defun part1 ()
  (let* ((warehouse-and-directions (read-input "day15input"))
         (warehouse (car warehouse-and-directions))
         (directions (cdr warehouse-and-directions)))
    (make-moves warehouse directions)))

(defun remap-warehouse (warehouse) 
  (let* ((rows (warehouse-rows warehouse))
         (init-columns (warehouse-columns warehouse))
         (init-locations (warehouse-locations warehouse))
         (locations (make-array (* (length init-locations) 2) :element-type 'character)))
    (loop for c across init-locations
          for i = 0 then (+ i 2)
          do (case c 
               (#\. 
                (setf (aref locations i) #\.)
                (setf (aref locations (+ i 1)) #\.))
               (#\#
                (setf (aref locations i) #\#)
                (setf (aref locations (+ i 1)) #\#))
               (#\@ 
                (setf (aref locations i) #\@)
                (setf (aref locations (+ i 1)) #\.))
               (#\O 
                (setf (aref locations i) #\[)
                (setf (aref locations (+ i 1)) #\]))))
    (make-warehouse :rows rows :columns (* 2 init-columns) :locations locations)))

(declaim (inline find-positions-to-move-into))
(defun find-positions-to-move-into (locations current-position offset)
  (declare (optimize (speed 3)) ((simple-array character) locations)
           (fixnum current-position offset))
  (let ((seen (make-hash-table))
        (q (make-queue current-position))
        (to-move-into nil))
    (loop while (non-empty q)
          for index = (poll q)
          for c = (aref locations index)
          do (case (aref locations (+ index offset))
               (#\# (return nil))
               (#\. )
               (#\] 
                (if (or (char= c #\@) (char= c #\[))
                    (progn (when (not (gethash (the fixnum (+ index offset -1)) seen))
                             (enqueue (the fixnum (+ index offset -1)) q)
                             (setf (gethash (the fixnum (+ index offset -1)) seen) t))
                           (when (not (gethash (the fixnum (+ index offset)) seen))
                             (enqueue (the fixnum (+ index offset)) q)
                             (setf (gethash (the fixnum (+ index offset)) seen) t)))
                    (when (not (gethash (the fixnum (+ index offset)) seen))
                      (enqueue (the fixnum (+ index offset)) q)
                      (setf (gethash (the fixnum (+ index offset)) seen) t))))
               (#\[
                (if (or (char= c #\@) (char= c #\]))
                    (progn 
                      (when (not (gethash (the fixnum (+ index offset)) seen))
                        (enqueue (the fixnum (+ index offset)) q)
                        (setf (gethash (the fixnum (+ index offset)) seen) t))
                      (when (not (gethash (the fixnum (+ index offset 1)) seen))
                        (enqueue (the fixnum (+ index offset 1)) q)
                        (setf (gethash (the fixnum (+ index offset 1)) seen) t)))
                    (when (not (gethash (the fixnum (+ index offset)) seen))
                      (enqueue (the fixnum (+ index offset)) q)
                      (setf (gethash (the fixnum (+ index offset)) seen) t)))))
             (push (the fixnum (+ index offset)) to-move-into)
          finally (return to-move-into))))

(declaim (inline shift-right))
(defun shift-right (locations current-position end-position)
  (declare (optimize (speed 3)) ((simple-array character) locations) 
           (fixnum current-position end-position))
  (loop for index from end-position downto (+ current-position 1)
        for previous-character = (aref locations (- index 1))
        do (setf (aref locations index) previous-character))
  (setf (aref locations current-position) #\.))

(declaim (inline shift-left))
(defun shift-left (locations current-position end-position)
  (declare (optimize (speed 3)) ((simple-array character) locations) 
           (fixnum current-position end-position))
  (loop for index from end-position to (- current-position 1)
        for previous-character = (aref locations (+ index 1))
        do (setf (aref locations index) previous-character))
  (setf (aref locations current-position) #\.))

(declaim (inline shift-vertical))
(defun shift-vertical (locations to-move-into offset)
  (declare (optimize (speed 3)) ((simple-array character) locations) (fixnum offset))
  (loop for index fixnum in to-move-into 
        do (setf (aref locations index)
                 (aref locations (- index offset)))
           (setf (aref locations (- index offset)) #\.)))

(defun move-with-wider-blocks (warehouse directions)
  (let ((columns (warehouse-columns warehouse))
        (locations (warehouse-locations warehouse)))
    (declare (optimize (speed 3)) ((simple-array character) locations))
    (loop with current fixnum = (find-start warehouse)
          for direction across directions
          for offset fixnum = (direction-offset direction columns)
          for adjacent = (aref locations (+ current offset))
          do (cond ((char= adjacent #\.) 
                    (setf (aref locations (+ current offset)) #\@)
                    (setf (aref locations current) #\.)
                    (incf current offset))
                   ((char= adjacent #\#))
                   ((= offset 1)
                    (let ((end-position
                            (find-next-available-square locations current offset)))
                      (when end-position 
                        (shift-right locations current end-position)
                        (incf current offset))))
                   ((= offset -1)
                    (let ((end-position
                            (find-next-available-square locations current offset)))
                      (when end-position
                        (shift-left locations current end-position)
                        (incf current offset))))
                   (t (let ((positions-to-move-into 
                              (find-positions-to-move-into locations current offset)))
                        (when positions-to-move-into
                          (shift-vertical locations positions-to-move-into offset)
                          (incf current offset))))))
    (loop for c across locations
          for index fixnum from 0
          when (char= c #\[)
            summing (gps-coordinate index columns))))

(defun part2 ()
  (let* ((warehouse-and-directions (read-input "day15input"))
         (warehouse (remap-warehouse (car warehouse-and-directions)))
         (directions (cdr warehouse-and-directions)))
    (move-with-wider-blocks warehouse directions)))
