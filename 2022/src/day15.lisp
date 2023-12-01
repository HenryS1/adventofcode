(defpackage :day15
  (:use 
   :cl 
   :aoc.functional
   :cl-ppcre 
   :trivia 
   :trivia.ppcre 
   :iterate 
   :arrow-macros
   :alexandria 
   :anaphora 
   :metabang-bind
   :aoc.datastructures)
  (:shadowing-import-from #:arrow-macros #:<>))

(in-package :day15)

(neat-lambda:enable-lambda-syntax)

(defun read-sensor-and-beacon (line)
  (match line ((ppcre "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"
                      (read s-x) (read s-y) (read b-x) (read b-y))
               (cons (cons (complex s-x s-y) (+ (abs (- s-x b-x)) 
                                                (abs (- s-y b-y))))
                     (complex b-x b-y)))))

(defun read-lines ()
  (iter (for line in-file "day15.input" using #'read-line)
    (collect (read-sensor-and-beacon line))))

(defun manhattan-distance (one other)
  (+ (abs (- (realpart one) (realpart other)))
     (abs (- (imagpart one) (imagpart other)))))

(defun sensor-bounds (row sensor)
  (bind (((coord . radius) sensor)
         (x (realpart coord))
         (y (imagpart coord))
         (delta (- radius (abs (- y row)))))
    (when (>= delta 0)
      (cons (- x delta) (+ x delta)))))

(defun min-sensor-range (row sensors)
  (reduce #'min (remove-if-not #'identity (mapcar #l(car (sensor-bounds row %sensor)) sensors))))

(defun max-sensor-range (row sensors)
  (reduce #'max (remove-if-not #'identity (mapcar #l(cdr (sensor-bounds row %sensor)) sensors))))

(defun beacons-in-row (beacons row)
  (remove-if-not #l(= (imagpart %beacon) row) beacons))

(defun sort-sensors (sensors row)
  (sort sensors #l(< %1 %2) :key #l(car (sensor-bounds row %sensor))))

(defun in-range (beacon)
  (lambda (sensor)
    (<= (manhattan-distance (car sensor) beacon) (cdr sensor))))

(defun count-impossible-locations (beacons sensors row)
  (bind ((s-min (min-sensor-range row sensors))
         (beacons-overlapping-sensors 
          (remove-if-not #l(some (in-range %beacon) sensors) 
                         (beacons-in-row beacons row))))
    (labels ((rec (x count sensors)
               (if (null sensors)
                   count
                   (bind (((s-min . s-max) (sensor-bounds row (car sensors)))
                          (coverage-count (max 0 (min (+ (- s-max x) 1) (+ 1 (- s-max s-min))))))
                     (if (> x s-max)
                         (rec x count (cdr sensors))
                         (rec (+ s-max 1) (+ count coverage-count) (cdr sensors)))))))
      (- (rec s-min 0 (sort-sensors (remove-if-not #l(sensor-bounds row %sensor) sensors) row)) 
         (length beacons-overlapping-sensors)))))

(defun part1 ()
  (bind ((input (read-lines))
         (sensors (mapcar #'car input))
         (beacons (remove-duplicates (mapcar #'cdr input))))
    (count-impossible-locations beacons sensors 2000000)))

(defun find-unobserved-block-in-row (start end row sensors)
  (bind ((bounds (sort (remove-if-not #'identity (mapcar #l(sensor-bounds row %sensor) sensors))
                       #'< :key #'car)))
    (labels ((rec (x remaining-bounds)
               (if (null remaining-bounds)
                   (when (<= x end) x)
                   (bind (((s-min . s-max) (car remaining-bounds)))
                     (cond ((> x s-max)
                            (rec x (cdr remaining-bounds)))
                           ((< x s-min) x)
                           (t (rec (+ s-max 1) (cdr remaining-bounds))))))))
      (rec start bounds))))

(defun find-unobserved-block (min-col max-col min-row max-row sensors)
  (iter (for row from min-row to max-row)
    (for unobserved-col = (find-unobserved-block-in-row min-col max-col row sensors))
    (when unobserved-col
      (return (cons unobserved-col row)))))

(defun part2 ()
  (bind ((sensors (mapcar #'car (read-lines)))
         ((x . y) (find-unobserved-block 0 4000000 0 4000000 sensors)))
    (+ (* x 4000000) y)))
