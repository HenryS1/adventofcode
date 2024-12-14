(defpackage :day14
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

(in-package :day14)

(defun ints (line) 
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" line)))

(defstruct robot (x 0 :type (signed-byte 8)) (y 0 :type (signed-byte 8)) (vx 0 :type (signed-byte 8)) (vy 0 :type (signed-byte 8)))

(defun parse-robot (line)
  (let ((stats (ints line)))
    (make-robot :x (car stats) :y (cadr stats) :vx (caddr stats) :vy (cadddr stats))))

(defun read-input (file)
  (map 'vector #'identity (with-open-file (f file)
                            (loop for line = (read-line f nil nil)
                                  while line collect (parse-robot line)))))

(defconstant width 101)
(defconstant height 103)
(defconstant middle-x 50)
(defconstant middle-y 51)

(declaim (inline move-robot))
(defun move-robot (robot times)
  (declare (fixnum times))
  (setf (robot-x robot) 
        (mod (the fixnum (+ (the fixnum (* times (robot-vx robot))) (robot-x robot))) width))
  (setf (robot-y robot)
        (mod (the fixnum (+ (the fixnum (* times (robot-vy robot))) (robot-y robot))) height)))

(defun robot-quadrant (robot)
  (when (and (/= (robot-x robot) middle-x)
             (/= (robot-y robot) middle-y))
    (cond ((and (< (robot-x robot) middle-x)
                (< (robot-y robot) middle-y))
           0)
          ((and (> (robot-x robot) middle-x)
                (< (robot-y robot) middle-y))
           1)
          ((and (< (robot-x robot) middle-x)
                (> (robot-y robot) middle-y))
           2)
          ((and (> (robot-x robot) middle-x)
                (> (robot-y robot) middle-y))
           3))))

(defun part1 ()
  (let ((robots (read-input "day14input")))
    (loop for robot across robots
          do (move-robot robot 100))
    (loop with first-quadrant = 0
          with second-quadrant = 0
          with third-quadrant = 0
          with fourth-quadrant = 0
          for robot across robots
          for quadrant = (robot-quadrant robot)
          do (case quadrant
               (0 (incf first-quadrant))
               (1 (incf second-quadrant))
               (2 (incf third-quadrant))
               (3 (incf fourth-quadrant))
               (t))
          finally (return (* first-quadrant second-quadrant third-quadrant fourth-quadrant)))))

(declaim (inline robot-in-tree))
(defun robot-in-tree (robot)
  (or (= (robot-x robot) middle-x)
      (and (< (robot-x robot) middle-x)
           (>= (+ (robot-x robot) (robot-y robot)) middle-x))
      (and (> (robot-x robot) middle-x)
           (<= (- (robot-x robot) (robot-y robot)) middle-x))))

(declaim (inline enough-robots-in-tree))
(defun enough-robots-in-tree (robots)
  (declare ((simple-array robot) robots))
  (let ((in-tree (count-if #'robot-in-tree robots)))
    (>= in-tree (* (length robots) 0.9))))

(defun print-robots (robots stream)
  (let ((robot-is-present (make-array (* height width) :element-type 'bit)))
   (loop for robot across robots 
         do (setf (aref robot-is-present (+ (* (robot-y robot) width) (robot-x robot))) 1))
   (loop for y from 0 to (- height 1)
         do (loop for x from 0 to (- width 1)
                  if (= (aref robot-is-present (+ (* y width) x)) 1)
                    do (format stream "X")
                  else do (format stream ".")
                  finally (format stream "~%")))))

(defun move-and-print (robots)
  (declare (optimize (speed 3)))
  (loop for i from 0 to 8000
        when (enough-robots-in-tree robots)
          do (format t "After ~a seconds~%" i)
             (print-robots robots t)
             (return i)
        do (loop for robot across robots do (move-robot robot 1))))

(defun part2 ()
  (let ((robots (read-input "day14input")))
    (move-and-print robots)))
