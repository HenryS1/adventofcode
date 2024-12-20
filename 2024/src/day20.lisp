(defpackage :day20
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :queue
   :fixnum-pq
   :anaphora 
   :metabang-bind)
  (:export 
   :calibration-value
   :total-calibration-value
   :digits-in-line
   :all-calibration-values))

(in-package :day20)

(defstruct track (rows 0 :type fixnum) (columns 0 :type fixnum) 
           (locations "" :type (simple-array character)))

(defun read-input (file)
  (let* ((lines (with-open-file (f file)
                  (loop for line = (read-line f nil nil)
                        while line collect line)))
         (rows (length lines))
         (columns (length (car lines)))
         (locations (apply #'concatenate 'string lines)))
    (make-track :rows rows :columns columns :locations locations)))

(defun find-start-and-end (track)
  (loop with start = nil
        with end = nil
        for index from 0
        for c across (track-locations track)
        if (char= c #\S)
          do (setf start index)
        else
          if (char= c #\E)
            do (setf end index)
        finally (return (cons start end))))

(defun find-shortest-path-between (track start end)
  (let* ((columns (track-columns track))
         (locations (track-locations track))
         (q (make-queue start))
         (distance-to (make-array (length locations) :element-type 'fixnum :initial-element -1)))
    (setf (aref distance-to start) 0)
    (loop while (non-empty q)
          for current = (poll q)
          for left = (- current 1)
          for left-char = (aref locations left)
          for right = (+ current 1)
          for right-char = (aref locations right)
          for up = (- current columns)
          for up-char = (aref locations up)
          for down = (+ current columns)
          for down-char = (aref locations down)
          when (= current end)
            do (return distance-to)
          when (and (char/= left-char #\#) (= (aref distance-to left) -1))
            do (enqueue left q)
               (setf (aref distance-to left) (+ (aref distance-to current) 1))
          when (and (char/= right-char #\#) (= (aref distance-to right) -1))
            do (enqueue right q)
               (setf (aref distance-to right) (+ (aref distance-to current) 1))
          when (and (char/= up-char #\#) (= (aref distance-to up) -1))
            do (enqueue up q)
               (setf (aref distance-to up) (+ (aref distance-to current) 1))
          when (and (char/= down-char #\#) (= (aref distance-to down) -1))
            do (enqueue down q)
               (setf (aref distance-to down) (+ (aref distance-to current) 1)))))

(defun find-shortest-paths-with-cheats (track distance-from-end target-difference)
  (let* ((rows (track-rows track))
         (columns (track-columns track))
         (locations (track-locations track))
         (start-and-end (find-start-and-end track))
         (start (car start-and-end))
         (end (cdr start-and-end))
         (q (make-queue start))
         (start-to-end (aref distance-from-end start))
         (distance-to (make-array (length locations) :element-type 'fixnum :initial-element -1))
         (cheats (make-array (length locations) :element-type 'bit)))
    (setf (aref distance-to start) 0)
    (format t "START TO END ~a~%" start-to-end)
    (loop while (non-empty q)
          for current = (poll q)
          for row = (floor current columns)
          for column = (mod current columns)
          for left = (- current 1)
          for left-char = (aref locations left)
          for right = (+ current 1)
          for right-char = (aref locations right)
          for up = (- current columns)
          for up-char = (aref locations up)
          for down = (+ current columns)
          for down-char = (aref locations down)
          for two-left = (when (> column 1) (- current 2))
          for two-left-char = (when two-left (aref locations two-left))
          for two-right = (when (< column (- columns 2)) (+ current 2))
          for two-right-char = (when two-right (aref locations two-right))
          for two-up = (when (> row 1) (- current (* columns 2)))
          for two-up-char = (when two-up (aref locations two-up))
          for two-down = (when (< row (- rows 2)) (+ current (* columns 2)))
          for two-down-char = (progn 
                                ;; (format t "ROW ~a COLUMN ~a ROWS ~a COLUMNS ~a TWO-DOWN ~a~%"
                                ;;         row column rows columns two-down)
                                (when two-down (aref locations two-down)))
          for distance-to-current = (aref distance-to current)
          when (= current end)
            do (return cheats)
          when (and two-left
                    (char/= two-left-char #\#)
                    (= (aref distance-to two-left) -1))
            do (let ((distance-to-end-with-cheat (+ distance-to-current 2
                                                    (aref distance-from-end two-left))))
                 (when (>= (- start-to-end distance-to-end-with-cheat) target-difference)
                   ;; (format t "DISTANCE FROM END ~a DISTANCE TO ~a FOUND CHEAT AT CURRENT ~a ~a TWO LEFT~%" 
                   ;;         (aref distance-from-end two-left) distance-to-current row column)
                   (setf (aref cheats left) 1)))
          when (and two-right
                    (char/= two-right-char #\#)
                    (= (aref distance-to two-right) -1))
            do (let ((distance-to-end-with-cheat (+ distance-to-current 2
                                                    (aref distance-from-end two-right))))
                 (when (>= (- start-to-end distance-to-end-with-cheat) target-difference)
                   ;; (format t "DISTANCE FROM END ~a DISTANCE TO ~a FOUND CHEAT AT CURRENT ~a ~a TWO RIGHT~%" 
                   ;;         (aref distance-from-end two-right) distance-to-current row column)
                   (setf (aref cheats right) 1)))
          when (and two-down
                    (char/= two-down-char #\#)
                    (= (aref distance-to two-down) -1))
            do (let ((distance-to-end-with-cheat (+ distance-to-current 2
                                                    (aref distance-from-end two-down))))
                 (when (>= (- start-to-end distance-to-end-with-cheat) target-difference)
                   ;; (format t "DISTANCE FROM END ~a DISTANCE TO ~a FOUND CHEAT AT CURRENT ~a ~a TWO DOWN~%" 
                   ;;         (aref distance-from-end two-down) distance-to-current row column)
                   (setf (aref cheats down) 1)))
          when (and two-up
                    (char/= two-up-char #\#)
                    (= (aref distance-to two-up) -1))
            do (let ((distance-to-end-with-cheat (+ distance-to-current 2
                                                    (aref distance-from-end two-up))))
                 (when (>= (- start-to-end distance-to-end-with-cheat) target-difference)
                   ;; (format t "DISTANCE FROM END ~a DISTANCE TO ~a FOUND CHEAT AT CURRENT ~a ~a TWO UP~%" 
                   ;;         (aref distance-from-end two-up) distance-to-current row column)
                   (setf (aref cheats up) 1)))
          when (and (char/= left-char #\#) (= (aref distance-to left) -1))
            do (enqueue left q)
               (setf (aref distance-to left) (+ (aref distance-to current) 1))
          when (and (char/= right-char #\#) (= (aref distance-to right) -1))
            do (enqueue right q)
               (setf (aref distance-to right) (+ (aref distance-to current) 1))
          when (and (char/= up-char #\#) (= (aref distance-to up) -1))
            do (enqueue up q)
               (setf (aref distance-to up) (+ (aref distance-to current) 1))
          when (and (char/= down-char #\#) (= (aref distance-to down) -1))
            do (enqueue down q)
               (setf (aref distance-to down) (+ (aref distance-to current) 1)))))

(defun part1 ()
  (let* ((track (read-input "day20input"))
         (start-and-end (find-start-and-end track))
         (start (car start-and-end))
         (end (cdr start-and-end))
         (distance-from-end (find-shortest-path-between track end start)))
    (reduce #'+ (find-shortest-paths-with-cheats track distance-from-end 100))
    ;; (loop for i in (list 2 4 6 8 10 12 20 36 38 40 64)
    ;;       collect (cons i (reduce #'+ (find-shortest-paths-with-cheats track distance-from-end i))))
    ))
