(defpackage :day10
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind
   :queue)
  (:export
   :next-number
   :previous-number
   :left
   :right
   :down
   :up))

(in-package :day10)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-grid ()
  (fmap #l(coerce %rows 'vector)
        (sep-by (many1 #l(not (newlinep %c))) (many1 #'newlinep))))

(defun parse-grid-from-file (filename)
  (parse-file filename (parse-grid)))

(defun get-row (coord)
  (realpart coord))

(defun get-col (coord)
  (imagpart coord))

(defun make-coord (row col)
  (complex row col))

(defun find-start (grid)
  (loop for row-number from 0 to (- (length grid) 1)
        for row across grid
        for column-coord = (loop for col-number from 0 to (- (length row) 1)
                                 for c across row
                                 when (char= c #\S)
                                   do (return col-number))
        when column-coord
          do (return (complex row-number column-coord))))

(defun next-direction (tile direction)
  (case tile
    (#\| (case direction
           (down 'down)
           (up 'up)))
    (#\- (case direction 
           (right 'right)
           (left 'left)))
    (#\L (case direction 
           (down 'right)
           (left 'up)))
    (#\J (case direction 
           (down 'left)
           (right 'up)))
    (#\7 (case direction 
           (up 'left)
           (right 'down)))
    (#\F (case direction
           (left 'down)
           (up 'right)))))

(defun in-bounds (grid coord)
  (bind((rows (length grid))
        (cols (length (aref grid 0))))
    (and (<= 0 (get-row coord) (- rows 1))
         (<= 0 (get-col coord) (- cols 1)))))

(defun next-coord (grid direction coord)
  (bind ((rows (length grid))
         (cols (length (aref grid 0))))
    (cond ((and (equal direction 'left) (> (get-col coord) 0))
           (let* ((next-tile (aref (aref grid (get-row coord)) (- (get-col coord) 1)))
                  (next-dir (next-direction next-tile direction)))
             (when (or (char= next-tile #\S) next-dir) 
               (make-coord (get-row coord) (- (get-col coord) 1)))))
          ((and (equal direction 'right) (< (get-col coord) (- cols 1)))
           (let* ((next-tile (aref (aref grid (get-row coord)) (+ (get-col coord) 1)))
                  (next-dir (next-direction next-tile direction)))
             (when (or (char= next-tile #\S) next-dir) 
               (make-coord (get-row coord) (+ (get-col coord) 1)))))
          ((and (equal direction 'up) (> (get-row coord) 0))
           (let* ((next-tile (aref (aref grid (- (get-row coord) 1)) (get-col coord)))
                  (next-dir (next-direction next-tile direction)))
             (when (or (char= next-tile #\S) next-dir)
               (make-coord (- (get-row coord) 1) (get-col coord)))))
          ((and (equal direction 'down) (< (get-row coord) (- rows 1)))
           (let* ((next-tile (aref (aref grid (+ (get-row coord) 1)) (get-col coord)))
                  (next-dir (next-direction next-tile direction)))
             (when (or (char= next-tile #\S) next-dir) 
               (make-coord (+ (get-row coord) 1) (get-col coord))))))))

(defun enqueue-starting-positions (coord grid distance-to direction edge-to)
  (bind ((q (make-queue))
         (rows (length grid))
         (cols (length (aref grid 0))))
    (when (and (> (get-col coord) 0)
               (next-direction 
                (aref (aref grid (get-row coord)) (- (get-col coord) 1))
                'left))
      (let ((next-coord (make-coord (get-row coord) (- (get-col coord) 1))))
       (enqueue next-coord q)
       (setf (aref distance-to (get-row coord) (- (get-col coord) 1)) 1)
       (setf (aref direction (get-row coord) (- (get-col coord) 1)) 
             (next-direction 
              (aref (aref grid (get-row coord)) (- (get-col coord) 1))
              'left))
       (setf (aref edge-to (get-row next-coord) (get-col next-coord)) coord)))
    (when (and (< (get-col coord) (- cols 1))
               (next-direction 
                (aref (aref grid (get-row coord)) (+ (get-col coord) 1))
                'right))
      (let ((next-coord (make-coord (get-row coord) (+ (get-col coord) 1))))
       (enqueue next-coord q)
       (setf (aref distance-to (get-row coord) (+ (get-col coord) 1)) 1)
       (setf (aref direction (get-row coord) (+ (get-col coord) 1)) 
             (next-direction 
              (aref (aref grid (get-row coord)) (+ (get-col coord) 1))
              'right))
        (setf (aref edge-to (get-row next-coord) (get-col next-coord)) coord)))
    (when (and (> (get-row coord) 0)
               (next-direction 
                (aref (aref grid (- (get-row coord) 1)) (get-col coord))
                'up))
      (let ((next-coord (make-coord (- (get-row coord) 1) (get-col coord)))) 
        (enqueue next-coord q)
        (setf (aref distance-to (- (get-row coord) 1) (get-col coord)) 1)
        (setf (aref direction (- (get-row coord) 1) (get-col coord)) 
              (next-direction 
               (aref (aref grid (get-row coord)) (+ (get-col coord) 1))
               'up))
        (setf (aref edge-to (get-row next-coord) (get-col next-coord)) coord)))
    (when (and (< (get-row coord) (- rows 1))
               (next-direction
                (aref (aref grid (+ (get-row coord) 1)) (get-col coord))
                'down))
      (let ((next-coord (make-coord (+ (get-row coord) 1) (get-col coord))))
        (enqueue next-coord q)
        (setf (aref distance-to (+ (get-row coord) 1) (get-col coord)) 1)
        (setf (aref direction (+ (get-row coord) 1) (get-col coord))
              (next-direction 
               (aref (aref grid (+ (get-row coord) 1)) (get-col coord))
               'down))
        (setf (aref edge-to (get-row next-coord) (get-col next-coord)) coord)))
    q))

(defun find-loop-path (next-coord current-coord edge-to)
  (let (one-side-coords
        other-side-coords)
    (loop for one-side = next-coord 
            then (aref edge-to (get-row one-side) (get-col one-side))
          while one-side
          do (push one-side one-side-coords))
    (loop for other-side = current-coord 
            then (aref edge-to (get-row other-side) (get-col other-side))
          while other-side
          do (push other-side other-side-coords))
    (append one-side-coords (butlast (reverse other-side-coords)))))

(defun find-loop (grid)
  (loop with rows = (length grid)
        with cols = (length (aref grid 0))
        with distance-to = (make-array (list rows cols) :initial-element -1)
        with directions = (make-array (list rows cols) :initial-element 'none)
        with edge-to = (make-array (list rows cols) :initial-element nil)
        with start-coord = (find-start grid)
        with q = (enqueue-starting-positions start-coord grid distance-to directions edge-to)
        while (not (empty q))
        for coord = (poll q)
        for tile = (aref (aref grid (get-row coord)) (get-col coord))
        for direction = (aref directions (get-row coord) (get-col coord))
        for distance = (aref distance-to (get-row coord) (get-col coord))
        for next-coord = (next-coord grid direction coord)
        when (and next-coord
                  (> (aref distance-to (get-row next-coord) (get-col next-coord)) 0))
          do (return (list next-coord 
                           (aref distance-to (get-row next-coord) (get-col next-coord))
                           (find-loop-path next-coord coord edge-to)))
        when next-coord
          do (enqueue next-coord q)
             (setf (aref distance-to (get-row next-coord) (get-col next-coord))
                   (+ distance 1))
             (setf (aref edge-to (get-row next-coord) (get-col next-coord)) coord)
             (setf (aref directions (get-row next-coord) (get-col next-coord))
                   (next-direction (aref (aref grid (get-row next-coord)) (get-col next-coord))
                                   direction))))

(defun part1 ()
  (find-loop (parse-grid-from-file "../tests/test-input10")))

(defun make-set (l)
  (loop with s = (make-hash-table)
        for e in l
        do (setf (gethash e s) t)
        finally (return s)))

(defun find-neighbours (grid coord loop-set seen)
  (let ((rows (length grid))
        (cols (length (aref grid 0)))
        neighbours)
    (when (> (get-row coord) 0)
      (let ((new-coord (make-coord (- (get-row coord) 1) (get-col coord))))
;        (format t "SEEN ~a LOOP-SET ~a~%" (gethash new-coord seen) (gethash new-coord loop-set))
        (when (and (not (gethash new-coord seen)) (not (gethash new-coord loop-set)))
          (push new-coord neighbours))))
    (when (< (get-row coord) (- rows 1))
      (let ((new-coord (make-coord (+ (get-row coord) 1) (get-col coord))))
        (when (and (not (gethash new-coord seen)) (not (gethash new-coord loop-set)))
          (push new-coord neighbours))))
    (when (> (get-col coord) 0)
      (let ((new-coord (make-coord (get-row coord) (- (get-col coord) 1))))
        (when (and (not (gethash new-coord seen)) (not (gethash new-coord loop-set)))
          (push new-coord neighbours))))
    (when (< (get-col coord) (- cols 1))
      (let ((new-coord (make-coord (get-row coord) (+ (get-col coord) 1))))
        (when (and (not (gethash new-coord seen)) (not (gethash new-coord loop-set)))
          (push new-coord neighbours))))
    neighbours))

(defun fill-connected (loop-set grid elem seen)
  (when (and (not (gethash elem loop-set)) (not (gethash elem seen)))
    (setf (gethash elem seen) t)
    (loop with stack = (list elem)
          while (not (null stack))
          for current = (pop stack)
          for neighbours = (find-neighbours grid current loop-set seen)
          do (loop for neighbour in neighbours
                   do (push neighbour stack)
                      (setf (gethash neighbour seen) t))
          finally (return seen))))

(defun find-left-side-offset (coord next-coord)
  (let ((difference (- next-coord coord)))
    (case difference
      (#c(0 -1) #c(1 0))
      (#c(0 1) #c(-1 0))
      (#c(1 0) #c(0 1))
      (#c(-1 0) #c(0 -1)))))

(defun contains-boundary (side grid)
  (loop with rows = (length grid)
        with cols = (length (aref grid 0))
        for coord being the hash-keys of side
        when (or (= (get-row coord) 0)
                 (= (get-row coord) (- rows 1))
                 (= (get-col coord) 0)
                 (= (get-col coord) (- cols 1)))
          do (return t)))

(defun find-inside-outside (grid the-loop)
  (loop with loop-set = (make-set the-loop)
        with left-side = (make-hash-table)
        with right-side = (make-hash-table)
        for remaining = (append (list (cadr the-loop) (car the-loop))
                                (reverse the-loop)) then (cdr remaining)
        while (cddr remaining)
        for previous-coord = (car remaining) 
        for coord = (cadr remaining)
        for next-coord = (caddr remaining)
        for first-left-side-offset = (find-left-side-offset previous-coord coord)
        for second-left-side-offset = (find-left-side-offset coord next-coord)
        for first-left-side-coord = (+ first-left-side-offset coord)
        for second-left-side-coord = (+ second-left-side-offset coord)
        for first-right-side-coord = (+ (- first-left-side-offset) coord)
        for second-right-side-coord = (+ (- second-left-side-offset) coord)
        when (in-bounds grid first-left-side-coord)
          do (fill-connected loop-set grid first-left-side-coord left-side)
        when (in-bounds grid second-left-side-coord)
          do (fill-connected loop-set grid second-left-side-coord left-side)
        when (in-bounds grid first-right-side-coord)
          do (fill-connected loop-set grid first-right-side-coord right-side)
        when (in-bounds grid second-right-side-coord)
          do (fill-connected loop-set grid second-right-side-coord right-side)
        finally (return (if (contains-boundary right-side grid)
                            (alexandria:hash-table-keys left-side)
                            (alexandria:hash-table-keys right-side)))))

(defun part2 ()
  (let* ((grid (parse-grid-from-file "input10"))
         (the-loop (car (last (find-loop grid)))))
    (length (find-inside-outside grid the-loop))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
