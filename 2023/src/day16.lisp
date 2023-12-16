(defpackage :day16
  (:use 
   :cl 
   :iterate 
   :anaphora 
   :alexandria
   :pears
   :metabang-bind
   :queue)
  (:export
   :read-mirrors-from-file
   :trace-light-path
   :find-maximum-lava-generation
   :make-coord
   :right
   :left 
   :up
   :down))

(in-package :day16)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-mirrors ()
  (fmap #l(coerce %lines 'vector)
        (sep-by (many1 #l(not (newlinep %c))) (many1 #'newlinep))))

(defun read-mirrors-from-file (filename)
  (parse-file filename (parse-mirrors)))

(defun make-coord (row col)
  (complex row col))

(defun get-row (coord)
  (realpart coord))

(defun get-col (coord)
  (imagpart coord))

(defun in-bounds (coord mirrors)
  (let ((rows (length mirrors))
        (cols (length (aref mirrors 0))))
    (and (<= 0 (get-row coord) (- rows 1))
         (<= 0 (get-col coord) (- cols 1)))))

(defun next-coord (direction coord)
  (case direction
    (right (cons (+ coord #c(0 1)) direction))
    (left (cons (- coord #c(0 1)) direction))
    (down (cons (+ coord #c(1 0)) direction))
    (up (cons (- coord #c(1 0)) direction))))

(defun neighbours (entry coord direction seen mirrors)
  (let ((candidates (case entry
                      (#\. (list (next-coord direction coord)))
                      (#\/ (list (case direction
                                   (up (next-coord 'right coord))
                                   (down (next-coord 'left coord))
                                   (left (next-coord 'down coord))
                                   (right (next-coord 'up coord)))))
                      (#\\ (list (case direction
                                   (up (next-coord 'left coord))
                                   (down (next-coord 'right coord))
                                   (left (next-coord 'up coord))
                                   (right (next-coord 'down coord)))))
                      (#\- (case direction
                             (up (list (next-coord 'right coord)
                                       (next-coord 'left coord)))
                             (down (list (next-coord 'right coord)
                                         (next-coord 'left coord)))
                             (right (list (next-coord 'right coord)))
                             (left (list (next-coord 'left coord)))))
                      (#\| (case direction
                             (up (list (next-coord 'up coord)))
                             (down (list (next-coord 'down coord)))
                             (right (list (next-coord 'up coord)
                                          (next-coord 'down coord)))
                             (left (list (next-coord 'up coord)
                                         (next-coord 'down coord))))))))
    (remove-if-not #l(and (in-bounds (car %candidate) mirrors) 
                          (not (gethash %candidate seen)))
                   candidates)))

(defun trace-light-path (mirrors start)
  (let* ((seen (make-hash-table :test 'equal))
         (stack (list start)))
    (setf (gethash start seen) t)
    (loop for next = (pop stack)
          while next
          for (coord . direction) = next
          for entry = (aref (aref mirrors (get-row coord)) (get-col coord))
          for neighbours = (neighbours entry coord direction seen mirrors)
          do (loop for neighbour in neighbours 
                   do (push neighbour stack)
                      (setf (gethash neighbour seen) t)))
    (loop with coord-set = (make-hash-table :test 'equal)
          for (coord . direction) being the hash-keys of seen
          do (setf (gethash coord coord-set) t)
          finally (return (hash-table-count coord-set)))))

(defun part1 ()
  (let ((mirrors (read-mirrors-from-file "../tests/test-input16")))
    (trace-light-path mirrors (cons (make-coord 0 0) 'right))))

(defun find-maximum-lava-generation (mirrors)
  (let ((rows (length mirrors))
        (cols (length (aref mirrors 0)))
        (max 0))
      (loop for col from 0 to (- cols 1)
            for from-top = (cons (make-coord 0 col) 'down)
            for from-bottom = (cons (make-coord (- rows 1) col) 'up)
            do (setf max (max max 
                              (trace-light-path mirrors from-top)
                              (trace-light-path mirrors from-bottom))))
    (loop for row from 0 to (- rows 1)
          for from-left = (cons (make-coord row 0) 'right)
          for from-right = (cons (make-coord row (- cols 1)) 'left)
          do (setf max (max max
                            (trace-light-path mirrors from-left)
                            (trace-light-path mirrors from-right))))
    max))

(defun part2 ()
  (let ((mirrors (read-mirrors-from-file "input16")))
    (find-maximum-lava-generation mirrors)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
