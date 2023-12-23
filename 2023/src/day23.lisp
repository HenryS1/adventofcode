(defpackage :day23
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

(in-package :day23)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-map ()
  (fmap #l(coerce %lines 'vector) (sep-by (many1 #l(not (newlinep %c))) (many1 #'newlinep))))

(defun read-map-from-file (filename)
  (parse-file filename (parse-map)))

(defun make-coord (row col)
  (complex row col))

(defun get-row (coord)
  (realpart coord))

(defun get-col (coord)
  (imagpart coord))

(defun find-start-and-end (map)
  (let ((start (loop for col-number from 0 for c across (aref map 0) 
                     when (char= c #\.) do (return (make-coord 0 col-number))))
        (end (loop for col-number from 0 for c across (aref map (- (length map) 1))
                   when (char= c #\.) do (return (make-coord (- (length map) 1) col-number)))))
    (cons start end)))

(defun in-bounds (coord map)
  (and (<= 0 (get-row coord) (- (length map) 1))
       (<= 0 (get-col coord) (- (length (aref map 0)) 1))))

(defun get-tile (map coord)
  (aref (aref map (get-row coord)) (get-col coord)))

(defun neighbours (previous coord map)
  (let ((up (- coord #c(1 0)))
        (down (+ coord #c(1 0)))
        (left (- coord #c(0 1)))
        (right (+ coord #c(0 1)))
        neighbours)
    (when (and (in-bounds up map) 
               (/= previous up)
               (or (char= (get-tile map up) #\.)
                   (char= (get-tile map up) #\^)))
      (push up neighbours))
    (when (and (in-bounds down map)
               (/= previous down)
               (or (char= (get-tile map down) #\.)
                   (char= (get-tile map down) #\v)))
      (push down neighbours))
    (when (and (in-bounds right map) 
               (/= previous right)
               (or (char= (get-tile map right) #\.)
                   (char= (get-tile map right) #\>)))
      (push right neighbours))
    (when (and (in-bounds left map)
               (/= previous left)
               (or (char= (get-tile map left) #\.)
                   (char= (get-tile map left) #\<)))
      (push left neighbours))
    neighbours))

(defun longest-path-to (start map destination)
  (labels ((recur (previous current steps)
             (if (= current destination)
                 steps
                 (loop for neighbour in (neighbours previous current map)
                       maximizing (recur current neighbour (+ steps 1))))))
    (recur start start 0)))

(defun part1 ()
  (bind ((the-map (read-map-from-file "input23"))
         ((start . end) (find-start-and-end the-map)))
    (longest-path-to start the-map end)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)

