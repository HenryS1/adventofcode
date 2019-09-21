(load "../2018/queue.lisp")

(declaim (optimize (debug 3)))

(defun count-bits (n)
  (loop while (> n 0)
     when (oddp n)
     count n
     do (setf n (ash n -1))))

(defparameter *input* 1350)

(defun openp (x y)
  (let ((n (+ (* x x) (* 3 x) (* 2 x y) y (* y y) *input*)))
    (evenp (count-bits n))))

(defun neighbours (current)
  (loop with nbrs = (list)
     with (start-x . start-y) = current
     for x from (- start-x 1) to (+ start-x 1)
     do (loop for y from (- start-y 1) to (+ start-y 1)
           when (and 
                 (or (= x start-x) (= y start-y))
                 (>= x 0) (>= y 0)
                 (or (/= x start-x) (/= y start-y))
                 (openp x y))
           do (push (cons x y) nbrs))
     finally (return nbrs)))

(defun find-path (destination stopping-condition)
  (let* ((start (cons 1 1))
         (q (make-queue))
         (steps-to (make-hash-table :test 'equal))
         (visited (make-hash-table :test 'equal)))
    (setf (gethash start steps-to) 0)
    (setf (gethash start visited) t)
    (enqueue start q)
    (labels ((bfs ()
               (when (non-empty q)
                 (loop with current = (poll q)
                      for neighbour in (neighbours current)
                      for steps-to-current = (gethash current steps-to)
                      until (funcall stopping-condition current destination steps-to)
                      when (not (gethash neighbour visited))
                      do (setf (gethash neighbour steps-to) 
                               (+ steps-to-current 1))
                        (setf (gethash neighbour visited) t)
                        (enqueue neighbour q))
                 (bfs))))
      (bfs)
      steps-to)))

(defun answer1 ()
  (let* ((destination '(31 . 39))
         (steps-to (find-path destination
                             (lambda (current destination steps-to)
                               (declare (ignore current))
                               (gethash destination steps-to)))))
    (gethash destination steps-to)))

(defun answer2 ()
  (let* ((destination '(31 . 39))
         (steps-to (find-path destination
                              (lambda (current destination steps-to)
                                (declare (ignore destination))
                                (= (gethash current steps-to) 51)))))
    (loop for steps being the hash-values of steps-to
       when (<= steps 50)
       count steps)))
