(load "utilities.lisp")
(ql:quickload :cl-ppcre)

(defparameter *input-file* "experimental-emergency-teleportation-input.txt")

(defun read-nano-bot (line)
  (multiple-value-bind (m rs)
      (cl-ppcre:scan-to-strings ".*<(-?\\d+),(-?\\d+),(-?\\d+)>,\\s+r=(\\d+)" line)
    (declare (ignore m))
    (map 'vector #'parse-integer rs)))

(defun read-input (filename)
  (coerce (read-lines filename #'read-nano-bot) 'vector))

(defun signal-strength (bot)
  (aref bot 3))

(defun find-strongest-bot (bots)
  (loop with best = nil
     with strength = 0
     for bot across bots
     when (> (signal-strength bot) strength)
     do (setf best bot
              strength (signal-strength bot))
     finally (return best)))

(defun manhattan-distance (one other)
  (+ (abs (- (aref one 0) (aref other 0)))
     (abs (- (aref one 1) (aref other 1)))
     (abs (- (aref one 2) (aref other 2)))))

(defun in-range (one)
  (lambda (other) 
    (<= (manhattan-distance one other) (signal-strength one))))

(defun solution-part-1 ()
  (let* ((bots (read-input *input-file*))
         (strongest (find-strongest-bot bots)))
    (length (remove-if-not (in-range strongest) bots))))

(defun find-bounding-box (nano-bots)
  (loop for bot across nano-bots 
     minimize (aref bot 0) into min-x
     minimize (aref bot 1) into min-y
     minimize (aref bot 2) into min-z
     maximize (aref bot 0) into max-x
     maximize (aref bot 1) into max-y
     maximize (aref bot 2) into max-z
     finally (return (vector min-x max-x min-y max-y min-z max-z))))

(defun find-cluster-bounding-box (cluster)
  (loop for bot being the hash-keys of cluster 
     minimize (aref bot 0) into min-x
     minimize (aref bot 1) into min-y
     minimize (aref bot 2) into min-z
     maximize (aref bot 0) into max-x
     maximize (aref bot 1) into max-y
     maximize (aref bot 2) into max-z
     finally (return (vector min-x max-x min-y max-y min-z max-z))))

(defun range-intersects (one other)
  (< (manhattan-distance one other)
     (+ (signal-strength one) (signal-strength other))))

(defun equal-bots (one other)
  (every #'= one other))

(defun find-cluster (bot nano-bots)
  (loop with cluster = (let ((table (make-hash-table))) 
                         (setf (gethash bot table) t)
                         table)
     for other across nano-bots
     when (and (not (equal-bots bot other))
               (range-intersects bot other))
     do (setf (gethash other cluster) t)
     finally (return cluster)))

(defun find-clusters (nano-bots)
  (loop with clusters = nil
     for bot across nano-bots
     when (not (some (lambda (cluster) (gethash bot cluster)) clusters))
     do (push (find-cluster bot nano-bots) clusters)
     finally (return clusters)))

(defun find-best-cluster (clusters)
  (loop with best = nil
     with best-size = 0
     for cluster in clusters
     when (> (hash-table-count cluster) best-size)
     do (setf best-size (hash-table-count cluster)
              best cluster)
     finally (return best)))

(defun up (point step-size bounding-box)
  (vector (aref point 0) (aref point 1) (min (+ (aref point 2) step-size) 
                                             (aref bounding-box 5))))

(defun down (point step-size bounding-box)
  (vector (aref point 0) (aref point 1) (max (- (aref point 2) step-size)
                                             (aref bounding-box 4))))

(defun north (point step-size bounding-box)
  (vector (min (+ (aref point 0) step-size)
               (aref bounding-box 1))
          (aref point 1) (aref point 2)))

(defun south (point step-size bounding-box)
  (vector (max (- (aref point 0) step-size)
               (aref bounding-box 0))
          (aref point 1) (aref point 2)))

(defun east (point step-size bounding-box)
  (vector (aref point 0) (min (+ (aref point 1) step-size)
                              (aref bounding-box 3))
          (aref point 2)))

(defun west (point step-size bounding-box)
  (vector (aref point 0) (max (- (aref point 1) step-size)
                              (aref bounding-box 2))
          (aref point 2)))

(defun neighbours (step-size point bounding-box)
  (vector (up point step-size bounding-box) (down point step-size bounding-box)
          (north point step-size bounding-box) (south point step-size bounding-box)
          (east point step-size bounding-box) (west point step-size bounding-box)))

(defun point-in-range (point bot)
  (< (manhattan-distance point bot)
     (signal-strength bot)))

(defun intersection-count (cluster)
  (lambda (point)
    (loop for bot being the hash-keys of cluster
                   count (point-in-range point bot))))

(defun sample-distribution (distribution distribution-total)
  (loop with selection = (random distribution-total)
     for entry across distribution
     when (> (car entry) selection)
     return (cdr entry)))

(defun find-next-point (step-size point cluster bounding-box better-weight worse-weight)
  (loop with nbrs = (map 'vector (lambda (point) 
                                   (cons (funcall (intersection-count cluster) point) point)) 
                         (neighbours step-size point bounding-box))
     with start-intersections = (funcall (intersection-count cluster) point)
     with distribution = (make-array (length nbrs) :fill-pointer 0 :adjustable t)
     with distribution-total = 0
     for neighbour across nbrs
     when (> (car neighbour) start-intersections)
     do (incf distribution-total (* (car neighbour) better-weight))
     when (<= (car neighbour) start-intersections)
     do (incf distribution-total (* (car neighbour) worse-weight))
     do (vector-push-extend (cons distribution-total (cdr neighbour)) distribution)
     finally (return (sample-distribution distribution distribution-total))))

(defun closer-to-origin (one other)
  (< (manhattan-distance one #(0 0 0)) (manhattan-distance other #(0 0 0))))

(defun find-best-intersection (points cluster)
  (loop with best-count = 0
     with best = nil
     with intersection-fun = (intersection-count cluster)
     for point across points
     for intersections = (funcall intersection-fun point)
     when (or (> intersections best-count)
              (and  
               (= intersections best-count)
               (closer-to-origin point best)))
     do (setf best-count intersections)
       (setf best point)
     finally (return (cons best best-count))))

(defun maximize-intersections-for-cluster (cluster)
  (maximize-intersections cluster (coerce (hash-keys cluster) 'vector)))

(defun maximize-intersections (cluster starting-points)
  (loop with best-intersections = (reduce #'max (map 'vector (intersection-count cluster)
                                                        starting-points))
     with best-point = nil
     with bounding-box = (find-bounding-box starting-points)
     with step-size = (floor (max (- (aref bounding-box 1) (aref bounding-box 0))
                                  (- (aref bounding-box 3) (aref bounding-box 2))
                                  (- (aref bounding-box 5) (aref bounding-box 4)))
                             2) 
     for better-weight = 100
     with worse-weight = 50
     for points = starting-points 
     then (map 'vector (lambda (point) (find-next-point (max 3 (random step-size)) point cluster 
                                                        bounding-box better-weight worse-weight))
               points)
     for i from 1 to 3000
;;     do (format t "POINTS ~a~%" points)
     when (= (mod i 10) 0)
     do 
       (destructuring-bind (point . intersections) (find-best-intersection points cluster)
         (format t "INTERSECTIONS ~a ~a~%" intersections point)
         (format t "BEST INTERSECTIONS ~a~%" best-intersections)
         (when (or (> intersections best-intersections)
                   (and (= intersections best-intersections)
                        (or (not best-point)
                            (closer-to-origin point best-point))))
           (format t "NEW BEST ~a ~a~%" intersections point)
           (setf best-point point
                 best-intersections intersections)
           (maximize-intersections-with-starting-neighbourhood cluster point)))
     finally (return best-intersections)))

(defun maximize-intersections-with-starting-neighbourhood (cluster point)
  (let ((starting-points (create-neighbourhood point cluster)))
    (maximize-intersections cluster starting-points)))

(defun create-neighbourhood (center cluster)
  (coerce (loop with bounding-box = (find-cluster-bounding-box cluster)
             with points = (vector center)
             for i from 1 to 500
             do (setf points (concatenate 
                              'vector points (neighbours (random 1000000) center bounding-box)))
             finally (return points)) 
          'vector))

(defun find-closest-to-origin (starting-point cluster intersections)
  (loop with previous = starting-point
     with bb = (find-cluster-bounding-box cluster) 
     for point = starting-point then (west (south (down point 1 bb) 1 bb) 1 bb)
     do (format t "INTERSECTIONS ~a ~a~%" point (funcall (intersection-count cluster) point))
     when (= intersections (funcall (intersection-count cluster) point))
     do (setf previous point)
     until (> intersections (funcall (intersection-count cluster) point))
     finally (return previous)))
