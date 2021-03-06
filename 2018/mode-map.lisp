(load "utilities.lisp")
(load "priority-queue.lisp")

(defun up (position) 
  (cons (car position) (1- (cdr position))))

(defun down (position)
  (cons (car position) (1+ (cdr position))))

(defun left (position)
  (cons (1- (car position)) (cdr position)))

(defun right (position)
  (cons (1+ (car position)) (cdr position)))

(defun erosion-level (depth position target erosion-levels)
  (if (gethash position erosion-levels)
      (gethash position erosion-levels)
      (let ((level (mod (+ (geologic-index depth position target erosion-levels) depth)
                        20183)))
        (setf (gethash position erosion-levels) level)
        level)))

(defun geologic-index (depth position target erosion-levels)
  (cond ((equal position target) 0)
        ((= (car position) 0)
         (* (cdr position) 48271))
        ((= (cdr position) 0)
         (* (car position) 16807))
        (t (* (erosion-level depth (left position) target erosion-levels)
              (erosion-level depth (up position) target erosion-levels)))))

(defun region-type (depth position target erosion-levels)
  (case (mod (erosion-level depth position target erosion-levels) 3)
    (0 'rocky)
    (1 'wet)
    (2 'narrow)))

(defun risk-level (start target depth)
  (loop with risk = 0
     with erosion-levels = (make-hash-table :test 'equal)
     for y from (cdr start) to (cdr target)
     do (loop for x from (car start) to (car target)
           do (case (region-type depth (cons x y) target erosion-levels)
                (wet (incf risk 1))
                (narrow (incf risk 2))))
     finally (return risk)))

(defun solution-part-1 ()
  (risk-level (cons 0 0) (cons 9 758) 8103))

(defparameter *equipment* '(climbing torch neither))

(defun compare-cons (one other)
  (< (car one) (car other)))

(defun available-equipment (position map)
  (case (funcall map position)
    (rocky '(climbing torch))
    (wet '(climbing neither))
    (narrow '(torch neither))))

(defun in-bounds (neighbour)
  (and (>= (car neighbour) 0)
       (>= (cdr neighbour) 0)))

(defun available (map position equipment)
  (member equipment (available-equipment position map)))

(defun enqueue-neighbours (q map el min-cost-to neighbour-fun path-to)
  (destructuring-bind (distance position equipment) el
    (loop for neighbour in (funcall neighbour-fun map position)
       do (let ((cost 1))
            (when (not (equal equipment (cdr neighbour)))
              (incf cost 7))
            (when (not (available map position (cdr neighbour)))
              (incf cost 7))
            (when (or (not (gethash neighbour min-cost-to))
                      (< (+ cost distance) (gethash neighbour min-cost-to)))
              (insert-pq (list (+ cost distance) (car neighbour) (cdr neighbour)) q)
              (setf (gethash neighbour min-cost-to) (+ cost distance))
              (setf (gethash neighbour path-to) (cons position equipment)))))))

(defun neighbours (map position)
  (loop with nbrs = nil
     for neighbour in (list (left position) (up position) (right position) (down position))
     when (in-bounds neighbour)
     do (loop for equipment in (available-equipment neighbour map)
           do (push (cons neighbour equipment) nbrs))
     finally (return nbrs)))

(defun distance (one other)
  (+ (abs (- (car one) (car other)))
     (abs (- (cdr one) (cdr other)))))

(defun min-cost (map target)
  (let* ((q (make-pq #'compare-cons))
         (min-cost-to (make-hash-table :test 'equal))
         (destination-key (cons target 'torch))
         (path-to (make-hash-table :test 'equal)))
    (insert-pq (list 0 (cons 0 0) 'torch) q)
    (setf (gethash (cons (cons 0 0) 'torch) min-cost-to) 0)
    (loop for el = (pop-pq q)
       while (and el (not (equal (cons (cadr el) (caddr el)) destination-key)))
       when (or (not (gethash (cons (cadr el) (caddr el)) min-cost-to))
                (<= (car el)
                    (gethash (cons (cadr el) (caddr el)) min-cost-to)))
       do (enqueue-neighbours q map el min-cost-to #'neighbours path-to))
    (gethash (cons target 'torch) min-cost-to)))

(defun print-map (map-fun bounds)
  (loop for y from 0 to (cdr bounds)
     do (loop for x from 0 to (car bounds)
           do (case (funcall map-fun (cons x y))
                (rocky (format t "."))
                (wet (format t "="))
                (narrow (format t "|"))))
       (format t "~%")))

(defun test-2 ()
  (let ((depth 510)
        (target (cons 10 10))
        (erosion-levels (make-hash-table :test 'equal)))
    (labels ((map-fun (position) (region-type depth position target erosion-levels)))
      (print-map #'map-fun (cons 15 15))
      (min-cost #'map-fun target))))

(defun solution-part-2 ()
  (let ((depth 8103)
        (target (cons 9 758))
        (erosion-levels (make-hash-table :test 'equal)))
    (labels ((map-fun (position) (region-type depth position (cons 100 1000) erosion-levels)))
      (min-cost #'map-fun target))))
