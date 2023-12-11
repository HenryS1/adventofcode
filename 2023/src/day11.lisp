(defpackage :day11
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind
   :queue)
  (:export
   :read-galaxies-from-file
   :find-empty-rows
   :find-empty-columns
   :find-star-coordinates
   :make-coord
   :adjust-star-coordinate
   :adjust-star-coordinates))

(in-package :day11)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-galaxies-from-file (filename)
  (parse-file filename (fmap #l(coerce %lines 'vector)
                             (sep-by (many1 #l(not (newlinep %c))) (many1 #'newlinep)))))

(defun find-empty-rows (galaxies)
  (loop for row across galaxies
        for row-number from 0
        when (every #p(char= #\.) row)
          collect row-number))

(defun find-empty-columns (galaxies)
  (loop with rows = (length galaxies)
        with cols = (length (aref galaxies 0))
        for col from 0 to (- cols 1)
        for is-empty = (loop for row from 0 to (- rows 1)
                           for c = (aref (aref galaxies row) col)
                           when (char= c #\#)
                             do (return nil)
                           finally (return t))
        when is-empty
          collect col))

(defun make-coord (row col)
  (complex row col))

(defun get-row (coord)
  (realpart coord))

(defun get-col (coord)
  (imagpart coord))

(defun find-star-coordinates (galaxies)
  (loop with star-coordinates = nil
        for row across galaxies
        for row-number from 0
        do (loop for c across row 
                 for col-number from 0
                 when (char= c #\#)
                   do (push (make-coord row-number col-number) star-coordinates))
        finally (return star-coordinates)))

(defun adjust-star-coordinate (star-coordinate empty-rows empty-columns &key (factor 1))
  (let ((adjusted-row 
          (loop with new-row = (get-row star-coordinate)
                for r in empty-rows 
                while (< r (get-row star-coordinate))
                do (incf new-row factor)
                finally (return new-row)))
        (adjusted-col 
          (loop with new-col = (get-col star-coordinate)
                for c in empty-columns
                while (< c (get-col star-coordinate))
                do (incf new-col factor)
                finally (return new-col))))
    (make-coord adjusted-row adjusted-col)))

(defun adjust-star-coordinates (star-coordinates empty-rows empty-columns &key (factor 1))
  (mapcar #l(adjust-star-coordinate %coordinate empty-rows empty-columns :factor factor)
          star-coordinates))

(defun manhattan-distance (one other)
  (let ((difference (- one other)))
    (+ (abs (get-row difference)) (abs (get-col difference)))))

(defun pairwise-distances (coordinates)
  (loop with distances = nil
        for remaining = coordinates then (cdr remaining)
        while (cdr remaining)
        for one = (car remaining)
        do (loop for other in (cdr remaining)
                 do (push (list (manhattan-distance one other) one other) distances))
        finally (return distances)))

(defun total-pairwise-distance (coordinates)
  (reduce #'+ (mapcar #'car (pairwise-distances coordinates))))

(defun part1 ()
  (bind ((galaxies (read-galaxies-from-file "input11"))
         (empty-rows (find-empty-rows galaxies))
         (empty-columns (find-empty-columns galaxies))
         (initial-star-coordinates (find-star-coordinates galaxies))
         (adjusted-star-coordinates (adjust-star-coordinates
                                     initial-star-coordinates empty-rows empty-columns)))
    (total-pairwise-distance adjusted-star-coordinates)))

(defun part2 ()
  (bind ((galaxies (read-galaxies-from-file "input11"))
         (empty-rows (find-empty-rows galaxies))
         (empty-columns (find-empty-columns galaxies))
         (initial-star-coordinates (find-star-coordinates galaxies))
         (adjusted-star-coordinates (adjust-star-coordinates
                                     initial-star-coordinates empty-rows empty-columns
                                     :factor 999999)))
    (total-pairwise-distance adjusted-star-coordinates)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
