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

(defun get-plot-mod (coord map)
  (aref (aref map (mod (get-row coord) (length map))) 
        (mod (get-col coord) 
             (length (aref map 0)))))

(defun neighbours-2 (coord visited map)
  (let ((up (- coord #c(1 0)))
        (down (+ coord #c(1 0)))
        (left (- coord #c(0 1)))
        (right (+ coord #c(0 1)))
        neighbours)
    (when (and (not (gethash up visited)) 
               (char/= (get-plot-mod up map) #\#))
      (push up neighbours))
    (when (and (not (gethash down visited))
               (char/= (get-plot-mod down map) #\#))
      (push down neighbours))
    (when (and (not (gethash left visited))
               (char/= (get-plot-mod left map) #\#))
      (push left neighbours))
    (when (and (not (gethash right visited))
               (char/= (get-plot-mod right map) #\#))
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

(defun quadrants-visited (map visited)
  (let ((rows (length map))
        (cols (length (aref map 0)))
        (distinct (make-hash-table)))
    (loop for coord being the hash-keys of visited
          for quadrant = (make-coord
                                 (floor (get-row coord) rows) (floor (get-col coord) cols))
          do (setf (gethash quadrant distinct)
                   (1+ (gethash quadrant distinct 0))) 
          finally (return distinct))))

;; (defun quadrant-is-full (map visited)
;;   (loop for cell being the hash-keys of visited))

(defun explore-map-2 (map max-steps)
  (let* ((visited (make-hash-table :test 'equal))
         (start (find-start map))
         (count-for-steps (make-hash-table)))
    (setf (gethash start visited) t)
    (loop with q = (make-queue (cons start 0))
          for dequeued = (poll q)
          while dequeued
          for (next . steps) = dequeued
          when (not (gethash steps count-for-steps))
            do (setf (gethash steps count-for-steps)
                     (gethash (- steps 2) count-for-steps 0))
          do (incf (gethash steps count-for-steps))
;          do (format t "STEPS ~a NEXT ~a PLOT ~a~%" steps next (get-plot-mod next map))
          when (< steps max-steps)
            do (loop for neighbour in (neighbours-2 next visited map)
                     do (setf (gethash neighbour visited) t)
                        (enqueue (cons neighbour (+ steps 1)) q))
          finally (return 
                    (progn 
;                      (format t "QUADRANTS VISITED ~a~%" (quadrants-visited map visited))
                      (loop for previous-steps = max-steps then (- previous-steps 2)
                            while (>= previous-steps 0)
                            when (gethash previous-steps count-for-steps)
                              do (return (cons 
                                          (quadrants-visited map visited)
                                          (gethash previous-steps count-for-steps)))))))))

(defun find-base-period (map)
  (let ((counts (make-hash-table)))
    (loop for i from 1 to 200
          for visited = (explore-map map i)
;          while (/= visited (gethash (- i 1) counts 0))
          do (setf (gethash i counts) visited)
;             (format t "VISITED ~a~%" visited)
          finally (return (- (gethash (- i 1) counts)
                             (gethash (- i 3) counts))))))

(defun part1 () 
  (let ((map (read-plots-from-file "input21")))
    (loop for i from 1 to 500
          for explored-previously = (or explored-at-i 0)
          for previous-quadrants = (or (and quadrants (hash-table-count quadrants)) 0)
          for (quadrants . explored-at-i) = (explore-map-2 map i)
          for previous-diff = (or diff 0)
          for diff = (- explored-at-i explored-previously)
          for second-order-diff = (- diff previous-diff)
;          when (/= (hash-table-count quadrants) previous-quadrants)
          ;; when (not (equal (gethash #c(1 1) quadrants)
          ;;                  (gethash #c(-1 -1) quadrants)))
          ;;   do (format t "~a ~a~%" (gethash #c(1 1) quadrants)
          ;;              (gethash #c(-1 -1) quadrants))
          when (gethash #c(-1 0) quadrants)
          do (format t "~a~%" ;i (- explored-at-i explored-previously) (hash-table-count quadrants)
                     ;(hash-table-alist quadrants)
                     (gethash #c(-1 0) quadrants))
          ;; do (format t "EXPLORED ~a DIFF ~a DIFF2 ~a QUADRANTS ~a MOD DIFF ~a~%" explored-at-i
          ;;            (- explored-at-i explored-previously)
          ;;            second-order-diff
          ;;            quadrants
          ;;            (mod (- explored-at-i explored-previously) 48))
          )))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)

