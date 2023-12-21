(defpackage :day21
  (:use 
   :cl 
   :iterate 
   :anaphora 
   :alexandria
   :pears
   :metabang-bind
   :priority-queue
   :queue)
  (:export
   ))

(in-package :day21)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-plots ()
  (fmap #l(coerce %lines 'vector)
        (sep-by (many1 #l(not (newlinep %c))) (many1 #'newlinep))))

(defun read-plots-from-file (filename)
  (parse-file filename (parse-plots)))

(defun make-coord (row col)
  (complex row col))

(defun get-row (coord)
  (realpart coord))

(defun get-col (coord)
  (imagpart coord))

(defun in-bounds (coord map)
  (let ((rows (length map))
        (cols (length (aref map 0))))
    (and (<= 0 (get-row coord) (- rows 1))
         (<= 0 (get-col coord) (- cols 1)))))

(defun get-plot (coord map)
  (aref (aref map (get-row coord)) (get-col coord)))

(defun neighbours (coord visited map steps)
  (let ((up (- coord #c(1 0)))
        (down (+ coord #c(1 0)))
        (left (- coord #c(0 1)))
        (right (+ coord #c(0 1)))
        neighbours)
    (when (and (in-bounds up map) (not (gethash (cons up (+ steps 1)) visited)) 
               (char/= (get-plot up map) #\#))
      (push up neighbours))
    (when (and (in-bounds down map) (not (gethash (cons down (+ steps 1)) visited))
               (char/= (get-plot down map) #\#))
      (push down neighbours))
    (when (and (in-bounds left map) (not (gethash (cons left (+ steps 1)) visited))
               (char/= (get-plot left map) #\#))
      (push left neighbours))
    (when (and (in-bounds right map) (not (gethash (cons right (+ steps 1)) visited))
               (char/= (get-plot right map) #\#))
      (push right neighbours))
    neighbours))

(defun find-start (map)
  (loop for row across map
        for row-number from 0
        for found-start = (loop for c across row
                                for col-number from 0
                                when (char= c #\S)
                                  do (return (make-coord row-number col-number)))
        when found-start 
          do (return found-start)))

(defun explore-map (map max-steps)
  (let* ((visited (make-hash-table :test 'equal))
         (start (find-start map))
         (destination-count 0))
    (setf (gethash start visited) t)
    (loop with q = (make-queue (cons start 0))
          for dequeued = (poll q)
          while dequeued
          for (next . steps) = dequeued
          when (= steps max-steps)
            do (incf destination-count)
;          do (format t "NEXT ~a PLOT ~a~%" next (get-plot next map))
          when (< steps max-steps)
            do (loop for neighbour in (neighbours next visited map steps)
                     for coord-and-steps = (cons neighbour (+ steps 1))
                     do (setf (gethash coord-and-steps visited) t)
                        (enqueue coord-and-steps q))
          finally (return destination-count))))

(defun part1 () 
  (let ((map (read-plots-from-file "input21")))
    (explore-map map 64)))



(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
