(load "../2018/queue.lisp")
(ql:quickload :ironclad)

(defun hash (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5
                             (ironclad:ascii-string-to-byte-array str))))

(defun in-bounds (coord)
  (and (>= (car coord) 0) (>= (cdr coord) 0)
       (< (car coord) 4) (< (cdr coord) 4)))

(defun up (coord) (cons (- (car coord) 1) (cdr coord)))
(defun down (coord) (cons (+ (car coord) 1) (cdr coord)))
(defun left (coord) (cons (car coord) (- (cdr coord) 1)))
(defun right (coord) (cons (car coord) (+ (cdr coord) 1)))

(defun openp (c) (find c "bcdef"))

(defun neighbours (seed current)
  (destructuring-bind (path-to . coord) current
    (let ((hsh (hash (concatenate 'string seed path-to)))
          nbrs)
      (mapc (lambda (nbr door direction)
              (when (and (in-bounds nbr) (openp (aref hsh door)))
                (push (cons (concatenate 'string path-to direction) nbr)
                      nbrs)))
            (list (up coord) (down coord) (left coord) (right coord))
            (list 0 1 2 3)
            (list "U" "D" "L" "R"))
      nbrs)))

(defun bfs (seed)
  (let ((q (make-queue '("" . (0 . 0))))
        (seen (make-hash-table :test 'equal)))
    (labels ((rec ()
               (when (non-empty q)
                 (if (equal (cdr (peek q)) '(3 . 3))
                     (car (peek q))
                     (progn 
                       (loop with current = (poll q)
                          for neighbour in (neighbours seed current)
                          for (path-to . coord) = neighbour
                          when (not (gethash neighbour seen))
                          do (setf (gethash neighbour seen) t)
                            (enqueue neighbour q))
                       (rec))))))
      (rec))))

