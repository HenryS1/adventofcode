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

(defun neighbours (previous coord map ignore-steep)
  (let ((up (- coord #c(1 0)))
        (down (+ coord #c(1 0)))
        (left (- coord #c(0 1)))
        (right (+ coord #c(0 1)))
        neighbours)
    (when (and (in-bounds up map) 
               (or (not previous) (not (gethash up previous)))
               (or (char= (get-tile map up) #\.)
                   (and ignore-steep (char/= (get-tile map up) #\#))
                   (char= (get-tile map up) #\^)))
      (push up neighbours))
    (when (and (in-bounds down map)
               (or (not previous) (not (gethash down previous)))
               (or (char= (get-tile map down) #\.)
                   (and ignore-steep (char/= (get-tile map down) #\#))
                   (char= (get-tile map down) #\v)))
      (push down neighbours))
    (when (and (in-bounds right map) 
               (or (not previous) (not (gethash right previous)))
               (or (char= (get-tile map right) #\.)
                   (and ignore-steep (char/= (get-tile map right) #\#))                   
                   (char= (get-tile map right) #\>)))
      (push right neighbours))
    (when (and (in-bounds left map)
               (or (not previous) (not (gethash left previous)))
               (or (char= (get-tile map left) #\.)
                   (and ignore-steep (char/= (get-tile map left) #\#))
                   (char= (get-tile map left) #\<)))
      (push left neighbours))
    neighbours))

(defun longest-path-to (start map destination &key (ignore-steep nil))
  (labels ((recur (previous current steps)
;             (format t "CURRENT ~a~%" current)
             (if (= current destination)
                 (progn 
;                   (format t "END~%")
                   steps)
                 (loop for neighbour in (neighbours previous current map ignore-steep)
                       do (setf (gethash current previous) t)
                       maximizing (recur previous neighbour (+ steps 1))
                       do (setf (gethash current previous) nil)))))
    (let ((previous (make-hash-table)))
      (setf (gethash start previous) t)
      (recur previous start 0))))


(defun part1 ()
  (bind ((the-map (read-map-from-file "input23"))
         ((start . end) (find-start-and-end the-map)))
    (longest-path-to start the-map end)))

;; (defun out-edges (start map destination)
;;   (let ((edges (make-hash-table)))
;;     (labels ((recur (current-start)
;;                (loop with visited = (make-hash-table)
;;                      with q = (make-queue current-start)
;;                      for  = (poll q)
;;                      while current
;;                      do (loop for neighbour in (neighbours nil current map t)
;;                               when (cadr neighbours) 
;;                                 do (setf ()))))))))

(defun compress-map (start map destination)
  (let ((distance-to (make-hash-table))
        (previous-vertex (make-hash-table))
        (visited (make-hash-table)))
    (setf (gethash start visited) t)
    (setf (gethash start distance-to) 0)
    (setf (gethash start previous-vertex) nil)
    (loop with q = (make-queue (list start 0 start))
          for (current distance previous) = (poll q)
          while current
          for neighbours = (neighbours visited current map t)
          do (loop for neighbour in neighbours
                   when (not (gethash neighbour visited))
                   do (enqueue (list neighbour (+ distance 1) 
                                     (if (cadr neighbours) current previous)) q)
                      (setf (gethash neighbour visited) t))
          do (format t "NEIGHBOURS ~a~%" neighbours)
          when (or (= current destination) (cadr neighbours))
            do (setf (gethash current distance-to) distance)
               (push previous (gethash current previous-vertex)))
    (cons distance-to previous-vertex)))

(defun part2 ()
  (bind ((the-map (read-map-from-file "../tests/test-input23"))
         ((start . end) (find-start-and-end the-map)))
;    (longest-path-to start the-map end :ignore-steep t)
    (compress-map start the-map end)))



(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)

