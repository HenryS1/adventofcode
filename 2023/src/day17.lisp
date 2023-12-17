(defpackage :day17
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
   :read-map-from-file
   :navigate
   :navigate-ultra))

(in-package :day17)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-map ()
  (fmap #l(coerce %lines 'vector)
        (sep-by (many1 #l(not (newlinep %c))) (many1 #'newlinep))))

(defun read-map-from-file (filename)
  (parse-file filename (parse-map)))

(defun make-coord (row col)
  (complex row col))

(defun get-row (coord)
  (realpart coord))

(defun get-col (coord)
  (imagpart coord))

(defun in-bounds (coord rows cols)
  (and (<= 0 (get-row coord) (- rows 1))
       (<= 0 (get-col coord) (- cols 1))))

(defun next-coord (coord direction)
  (case direction 
    (right (+ coord #c(0 1)))
    (left (- coord #c(0 1)))
    (down (+ coord #c(1 0)))
    (up (- coord #c(1 0)))))

(defun left-turn (direction)
  (case direction
    (right 'up)
    (left 'down)
    (up 'left)
    (down 'right)))

(defun right-turn (direction)
  (case direction
    (right 'down)
    (left 'up)
    (up 'right)
    (down 'left)))

(defun make-key (direction count-remaining coord)
  (list direction count-remaining coord))

(defun get-heat-loss (the-map row col)
  (digit-char-p (aref (aref the-map row) col)))

(defun enqueue-neighbours (visited the-map pq heat-loss-so-far
                           coord direction count-remaining rows cols)
  (when (> count-remaining 0)
    (let* ((next (next-coord coord direction))           
           (key (make-key direction (- count-remaining 1) next)))
      (when (and (in-bounds next rows cols) (not (gethash key visited)))
        (let ((next-heat-loss (+ heat-loss-so-far 
                              (digit-char-p 
                               (aref (aref the-map (get-row next)) (get-col next))))))
          (setf (gethash key visited) t)
          (insert-pq (list next-heat-loss direction (- count-remaining 1) next) pq)))))
  (let* ((next-direction (left-turn direction))
         (next-left-turn (next-coord coord next-direction))         
         (key (make-key next-direction 2 next-left-turn)))
    (when (and (in-bounds next-left-turn rows cols) (not (gethash key visited)))
      (let ((next-heat-loss (+ heat-loss-so-far 
                               (digit-char-p (aref (aref the-map 
                                                         (get-row next-left-turn))
                                                   (get-col next-left-turn))))))
        (setf (gethash key visited) t)
        (insert-pq (list next-heat-loss next-direction 2 next-left-turn) pq))))
  (let* ((next-direction (right-turn direction))
         (next-right-turn (next-coord coord next-direction))         
         (key (make-key next-direction 2 next-right-turn)))
    (when (and (in-bounds next-right-turn rows cols) (not (gethash key visited)))
      (let ((next-heat-loss (+ heat-loss-so-far
                               (digit-char-p (aref (aref the-map
                                                         (get-row next-right-turn))
                                                   (get-col next-right-turn))))))
        (setf (gethash key visited) t)
        (insert-pq (list next-heat-loss next-direction 2 next-right-turn) pq)))))

(defun navigate (the-map)
  (let* ((visited (make-hash-table :test 'equal))
         (pq (make-pq #l(< (car %1one) (car %2other))))
         (rows (length the-map))
         (cols (length (aref the-map 0)))
         (destination (make-coord (- rows 1) (- cols 1))))
    (insert-pq (list (get-heat-loss the-map 0 1) 'right 2 (make-coord 0 1)) pq)
    (insert-pq (list (get-heat-loss the-map 1 0) 'down 2 (make-coord 1 0)) pq)
    (setf (gethash (list 'right 2 (make-coord 0 1)) visited) t)
    (setf (gethash (list 'down 2 (make-coord 1 0)) visited) t)
    (loop while (pq-nonempty pq)
          for (heat-loss-so-far direction count-remaining coord) = (pop-pq pq)
          for key = (list direction count-remaining coord)
          when (= coord destination)
            do (return heat-loss-so-far)
          do (enqueue-neighbours visited the-map pq heat-loss-so-far
                                 coord direction count-remaining rows cols))))

(defun part1 ()
  (let ((the-map (read-map-from-file "input17")))
    (navigate the-map)))

(defun enqueue-neighbours-ultra (visited the-map pq heat-loss-so-far
                                 coord direction count-moved rows cols)
  (when (< count-moved 10)
    (let* ((next (next-coord coord direction))           
           (key (make-key direction (+ count-moved 1) next)))
      (when (and (in-bounds next rows cols) (not (gethash key visited)))
        (let ((next-heat-loss (+ heat-loss-so-far 
                                 (get-heat-loss the-map (get-row next) (get-col next)))))
          (setf (gethash key visited) t)
          (insert-pq (list next-heat-loss direction (+ count-moved 1) next) pq)))))
  (when (>= count-moved 4)
    (let* ((next-direction (left-turn direction))
           (next-left-turn (next-coord coord next-direction))         
           (key (make-key next-direction 1 next-left-turn)))
      (when (and (in-bounds next-left-turn rows cols) (not (gethash key visited)))
        (let ((next-heat-loss (+ heat-loss-so-far 
                                 (get-heat-loss the-map
                                                (get-row next-left-turn)
                                                (get-col next-left-turn)))))
          (setf (gethash key visited) t)
          (insert-pq (list next-heat-loss next-direction 1 next-left-turn) pq))))
    (let* ((next-direction (right-turn direction))
           (next-right-turn (next-coord coord next-direction))         
           (key (make-key next-direction 1 next-right-turn)))
      (when (and (in-bounds next-right-turn rows cols) (not (gethash key visited)))
        (let ((next-heat-loss (+ heat-loss-so-far
                                 (get-heat-loss the-map
                                                (get-row next-right-turn)
                                                (get-col next-right-turn)))))
          (setf (gethash key visited) t)
          (insert-pq (list next-heat-loss next-direction 1 next-right-turn) pq))))))

(defun navigate-ultra (the-map)
  (let* ((visited (make-hash-table :test 'equal))
         (pq (make-pq #l(< (car %1one) (car %2other))))
         (rows (length the-map))
         (cols (length (aref the-map 0)))
         (destination (make-coord (- rows 1) (- cols 1))))
    (insert-pq (list (get-heat-loss the-map 0 1) 'right 1 (make-coord 0 1)) pq)
    (insert-pq (list (get-heat-loss the-map 1 0) 'down 1 (make-coord 1 0)) pq)
    (setf (gethash (list 'right 1 (make-coord 0 1)) visited) t)
    (setf (gethash (list 'down 1 (make-coord 1 0)) visited) t)
    (loop while (pq-nonempty pq)
          for (heat-loss-so-far direction count-moved coord) = (pop-pq pq)
          when (= coord destination)
            do (return heat-loss-so-far)
          do (enqueue-neighbours-ultra visited the-map pq heat-loss-so-far
                                       coord direction count-moved rows cols))))

(defun part2 ()
  (let ((the-map (read-map-from-file "input17")))
    (navigate-ultra the-map)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
