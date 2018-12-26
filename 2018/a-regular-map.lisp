(load "queue.lisp")
(load "utilities.lisp")

(defun up (pos)
  (cons (1- (car pos)) (cdr pos)))

(defun down (pos)
  (cons (1+ (car pos)) (cdr pos)))

(defun left (pos)
  (cons (car pos) (1- (cdr pos))))

(defun right (pos)
  (cons (car pos) (1+ (cdr pos))))

(defun add-routes-to-map (index instruction position map)
  (loop for ind = index then (1+ ind)
     until (or (char= (aref instruction ind) #\$)
               (char= (aref instruction ind) #\|)
               (char= (aref instruction ind) #\)))
     do (case (aref instruction ind) 
          (#\W (push (left position) (gethash position map))
               (setf position (left position)))
          (#\E (push (right position) (gethash position map))
               (setf position (right position)))
          (#\N (push (up position) (gethash position map))
               (setf position (up position)))
          (#\S (push (down position) (gethash position map))
               (setf position (down position)))
          (#\( (loop for i = (1+ ind) then (add-routes-to-map i instruction position map)
                  when (char= (aref instruction i) #\|)
                  do (incf i)
                  until (char= (aref instruction i) #\))
                  finally (setf ind i))))
     finally (return ind)))

(defparameter *input-file* "a-regular-map-input.txt")
(defparameter *test-input-file* "a-regular-map-test-input.txt")

(defun test-1 ()
  (let ((instruction "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
        (map (make-hash-table :test 'equal)))
    (add-routes-to-map 1 instruction (cons 0 0) map)
    (find-furthest-point (cons 0 0) map)))

(defun find-distance-to-points (start map)
  (let ((seen (make-hash-table :test 'equal))
        (distance-to (make-hash-table :test 'equal))
        (q (make-queue)))
    (setf (gethash start distance-to) 0)
    (setf (gethash start seen) t)
    (labels ((rec (position)
               (loop for neighbour in (gethash position map)
                  when (not (gethash neighbour seen))
                  do (setf (gethash neighbour seen) t)
                    (setf (gethash neighbour distance-to) 
                          (1+ (gethash position distance-to)))
                    (enqueue neighbour q)
                  finally (let ((q-next (poll q)))
                            (when q-next
                              (rec q-next))))))
      (rec start)
      distance-to)))

(defun maximize-distance (distance-to)
  (loop for distance being the hash-values of distance-to
     maximize distance))

(defun solution-part-1 ()
  (let ((instruction (car (read-lines *input-file*)))
        (map (make-hash-table :test 'equal)))
    (add-routes-to-map 1 instruction (cons 0 0) map)
    (maximize-distance (find-distance-to-points (cons 0 0) map))))

(defun count-points-with-high-distance (distance-to)
  (loop for distance being the hash-values of distance-to
     count (>= distance 1000)))

(defun solution-part-2 ()
  (let ((instruction (car (read-lines *input-file*)))
        (map (make-hash-table :test 'equal)))
    (add-routes-to-map 1 instruction (cons 0 0) map)
    (count-points-with-high-distance (find-distance-to-points (cons 0 0) map))))
