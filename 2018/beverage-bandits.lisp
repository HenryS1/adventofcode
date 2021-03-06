(load "queue.lisp")
(load "utilities.lisp")

(defun right (coord)
  (cons (car coord) (1+ (cdr coord))))

(defun left (coord)
  (cons (car coord) (1- (cdr coord))))

(defun up (coord)
  (cons (1- (car coord)) (cdr coord)))

(defun down (coord)
  (cons (1+ (car coord)) (cdr coord)))

(defparameter *input-file* "beverage-bandits-input.txt")
(defparameter *test-input-file* "beverage-bandits-test-input.txt")

(defun parse-map (text-map)
  (let ((coord-map (make-hash-table :test 'equal))
        (units (make-hash-table :test 'equal)))
    (loop for text-row in text-map
       for row = 0 then (1+ row)
       do (loop for c across text-row
             for col = 0 then (1+ col)
             when (or (char= c #\.) (char= c #\G) (char= c #\E))
             do (let ((coord (cons row col)))
                  (setf (gethash coord coord-map) t))
             when (char= c #\G)
             do (setf (gethash (cons row col) units) (cons #\G 200))
             when (char= c #\E)
             do (setf (gethash (cons row col) units) (cons #\E 200))))
    (loop for coord being the hash-keys of coord-map
       do (add-neighbours coord coord-map))
    (loop for coord being the hash-keys of coord-map
       do (setf (gethash coord coord-map) (reverse (gethash coord coord-map))))
    (values coord-map units)))

(defun print-map (coord-map units)
  (loop for row from 0 to 31
     do (loop for col from 0 to 31 
           do (let ((coord (cons row col)))
                (if (gethash coord units)
                    (format t "~a" (car (gethash coord units)))
                    (if (gethash coord coord-map)
                        (format t ".")
                        (format t "#")))))
       (format t "~%")))

(defun read-map (filename)
  (let ((text-map (read-lines filename)))
    (parse-map text-map)))

(defun add-neighbours (coord coord-map)
  (setf (gethash coord coord-map) nil)
  (when (gethash (up coord) coord-map)
    (push (up coord) (gethash coord coord-map)))
  (when (gethash (left coord) coord-map)
    (push (left coord) (gethash coord coord-map)))
  (when (gethash (right coord) coord-map)
    (push (right coord) (gethash coord coord-map)))
  (when (gethash (down coord) coord-map)
    (push (down coord) (gethash coord coord-map))))

(defun neighbours (coord coord-map)
  (gethash coord coord-map))

(defun is-enemy (one other)
  (char/= (car one) (car other)))

(defun first-step (start coord path-to)
  (if (or (equal start coord) (equal start (gethash coord path-to)))
      coord
      (first-step start (gethash coord path-to) path-to)))

(defun reading-order (one other)
  (or (< (car one) (car other))
      (and (= (car one) (car other))
           (< (cdr one) (cdr other)))))

(defun find-place-to-move (unit start coord-map units)
  (let ((q (make-queue))
        (seen (make-hash-table :test 'equal))
        (path-to (make-hash-table :test 'equal))
        (distance-to (make-hash-table :test 'equal))
        (min-distance 999999999)
        closest)
    (setf (gethash start distance-to) 0)
    (labels ((bfs (coord)
               (loop for neighbour in (neighbours coord coord-map)
                  do (let ((possible-unit (gethash neighbour units)))
                       (when (and possible-unit (is-enemy unit possible-unit))
                         (setf min-distance (gethash coord distance-to))
                         (push coord closest)))
                  when (and (not (gethash neighbour seen))
                            (not (gethash neighbour units)))
                  do (setf (gethash neighbour seen) t)
                    (setf (gethash neighbour path-to) coord)
                    (setf (gethash neighbour distance-to) (1+ (gethash coord distance-to)))
                    (enqueue neighbour q))
               (let ((next (poll q)))
                 (when (and next (<= (gethash next distance-to) min-distance))
                   (bfs next)))))
      (bfs start)
      (if closest
          (first-step start (car (sort closest #'reading-order)) path-to)
          start))))

(defun compare-hit-points (units)
  (lambda (one other)
    (or (< (cdr (gethash one units)) (cdr (gethash other units)))
        (and (= (cdr (gethash one units)) (cdr (gethash other units)))
             (reading-order one other)))))

(defun find-adjacent-enemy (unit coord units coord-map)
  (let (enemies)
    (loop for neighbour in (neighbours coord coord-map)
       when (and (gethash neighbour units)
                 (is-enemy unit (gethash neighbour units)))
       do (push neighbour enemies))
    (car (sort enemies (compare-hit-points units)))))

(defun move (first-step unit coord units)
  (when (and first-step (not (equal first-step (car unit))))
      (remhash coord units)
      (setf (gethash first-step units) unit))
    first-step)

(defun attack (target target-coord units elf-attack-power)
  (if (char= (car target) #\G)
      (decf (cdr target) elf-attack-power)
      (decf (cdr target) 3))
  (when (<= (cdr target) 0)
    (remhash target-coord units)
    (when (char= (car target) #\E)
      t)))

(defun has-enemies (units)
  (let (sides)
    (loop for unit being the hash-values of units
       do (setf sides (adjoin (car unit) sides))
         (when (> (length sides) 1)
           (return-from has-enemies t)))
    nil))

(defun tick-round (coord-map units start-round elf-attack-power &optional (return-early nil))
  (let ((unit-order (sort (hash-keys units) #'reading-order))
        elf-died
        (moved-to (make-hash-table :test 'equal)))
    (loop for coord in unit-order
       when (not (has-enemies units))
       do (return-from tick-round (values start-round nil))
       when (and (gethash coord units) (not (gethash coord moved-to)))
       do (let ((first-step (find-place-to-move (gethash coord units) coord coord-map units)))
            (move first-step (gethash coord units) coord units)
            (setf (gethash first-step moved-to) t)
            (let ((target-coord (find-adjacent-enemy
                                 (gethash first-step units) first-step units coord-map)))
              (when target-coord
                (setf elf-died (attack (gethash target-coord units) 
                                       target-coord units elf-attack-power)))))
       when (and elf-died return-early)
       do (return-from tick-round (values (1+ start-round) t)))
    (values (1+ start-round) nil)))

(defun tick-until-battle-is-finished (coord-map units)
  (loop while (has-enemies units)
     for round = 0 then (tick-round coord-map units round 3)
     finally (return round)))

(defun total-health (units)
  (loop for unit being the hash-values of units
       sum (cdr unit) into total-health
       finally (return total-health)))

(defun test ()
  (multiple-value-bind (coord-map units) (read-map *test-input-file*)
    (let ((final-round (tick-until-battle-is-finished coord-map units))
          (total-health (total-health units)))
      (* final-round total-health))))

(defun solution-part-1 ()
  (multiple-value-bind (coord-map units) (read-map *input-file*)
    (let ((final-round (tick-until-battle-is-finished coord-map units))
          (total-health (total-health units)))
      (* final-round total-health))))

(defun copy-units (units)
  (let ((new-units (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k new-units) (cons (car v) (cdr v)))) units)
    new-units))

(defun find-minimal-power-increase-for-elves (coord-map units)
  (let ((elf-attack-power 4))
    (labels ((try-finish-battle ()
               (let ((units (copy-units units))
                     (round 0))
                 (loop while (has-enemies units)
                    do (multiple-value-bind (next-round elf-died) 
                           (tick-round coord-map units round elf-attack-power t)
                         (setf round next-round)
                         (when elf-died
                           (incf elf-attack-power)
                           (return-from try-finish-battle (try-finish-battle)))))
                 (values elf-attack-power round (total-health units)))))
      (try-finish-battle))))

(defun test-2 ()
  (multiple-value-bind (coord-map units) (read-map *test-input-file*)
    (multiple-value-bind (elf-attack-power final-round total-health) 
        (find-minimal-power-increase-for-elves coord-map units)
      (declare (ignore elf-attack-power))
      (* final-round total-health))))

(defun solution-part-2 ()
  (multiple-value-bind (coord-map units) (read-map *input-file*)
    (multiple-value-bind (elf-attack-power final-round total-health) 
        (find-minimal-power-increase-for-elves coord-map units)
      (declare (ignore elf-attack-power))
      (* final-round total-health))))
