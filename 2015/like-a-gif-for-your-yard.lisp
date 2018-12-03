(defun adjacent (one other)
  (destructuring-bind ((one-row . one-col) (other-row . other-col)) (list one other)
    (and (not (equal one other))
         (< (abs (- one-row other-row)) 2)
         (< (abs (- one-col other-col)) 2))))

(defun count-neighbours (one lights)
  (let ((count 0))
    (loop for other being the hash-keys of lights
       when (adjacent one other)
       do (incf count))
    count))

(defun stays-on (light lights)
  (let ((neighbour-count (count-neighbours light lights)))
    (or (= neighbour-count 2) (= neighbour-count 3))))

(defun turns-on (light lights)
  (let ((neighbour-count (count-neighbours light lights)))
    (= neighbour-count 3)))

(defun in-bounds (light rows cols)
  (destructuring-bind (row . col) light
    (and (> rows row -1)
         (> cols col -1))))

(defun neighbours (light rows cols)
  (destructuring-bind (row . col) light
    (remove-if-not (lambda (neighbour) (in-bounds neighbour rows cols))
                   (list (cons (1+ row) col)
                         (cons (1+ row) (1+ col))
                         (cons (1+ row) (1- col))
                         (cons row (1+ col))
                         (cons row (1- col))
                         (cons (1- row) col)
                         (cons (1- row) (1+ col))
                         (cons (1- row) (1- col))))))

(defun off-neighbours (lights rows cols)
  (let ((off (make-hash-table :test 'equal)))
    (loop for light being the hash-keys of lights
       do (loop for neighbour in (neighbours light rows cols)
             when (not (gethash neighbour lights))
             do (setf (gethash neighbour off) t)))
    off))

(defun on-next-turn (lights rows cols)
  (let ((off (off-neighbours lights rows cols))
        (next-lights (make-hash-table :test 'equal)))
    (loop for light being the hash-keys of lights
       when (stays-on light lights)
       do (setf (gethash light next-lights) t))
    (loop for light being the hash-keys of off
       when (turns-on light lights)
       do (setf (gethash light next-lights) t))
    next-lights))

(defun on-next-turn-with-corners (lights rows cols)
  (let ((off (off-neighbours lights rows cols))
        (next-lights (make-hash-table :test 'equal)))
    (loop for light being the hash-keys of lights
       when (stays-on light lights)
       do (setf (gethash light next-lights) t))
    (loop for light being the hash-keys of off
       when (turns-on light lights)
       do (setf (gethash light next-lights) t))
    (setf (gethash '(0 . 0) next-lights) t)
    (setf (gethash (cons 0 (1- cols)) next-lights) t)
    (setf (gethash (cons (1- rows) 0) next-lights) t)
    (setf (gethash (cons (1- rows) (1- cols)) next-lights) t)
    next-lights))

(defun set-lights (lines)
  (let ((lights (make-hash-table :test 'equal)))
    (loop for line in lines
       for row = 0 then (1+ row)
       do (loop for c across line
             for col = 0 then (1+ col)
             when (char= c #\#)
             do (setf (gethash (cons row col) lights) t)))
    lights))

(defun read-lights ()
  (with-open-file (f "like-a-gif-for-your-yard-input.txt")
    (when f
      (set-lights 
       (loop for line = (read-line f nil nil)
          while line
          collect line)))))

(let ((lights (set-lights (list "##.#.#"
                                "...##."
                                "#....#"
                                "..#..."
                                "#.#..#"
                                "####.#"))))
  (loop for i from 1 to 5 do (setf lights (on-next-turn lights 6 6)))
  (format t "~a~%" (hash-table-count lights)))

(defun solution-part-1 () 
  (let ((lights (read-lights)))
    (loop for i from 1 to 100
       do (setf lights (on-next-turn lights 100 100)))
    (hash-table-count lights)))

(defun solution-part-2 ()
  (let ((lights (read-lights)))
    (setf (gethash '(0 . 0) lights) t)
    (setf (gethash (cons 0 99) lights) t)
    (setf (gethash (cons 99 0) lights) t)
    (setf (gethash (cons 99 99) lights) t)
    (loop for i from 1 to 100
       do (setf lights (on-next-turn-with-corners lights 100 100)))
    (hash-table-count lights)))
