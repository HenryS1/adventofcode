(defun get-from-grid (row col grid)
  (aref (aref grid row) col))

(defun not-parallel-vertical (one other)
  (not (or (and (char= one #\-)
                (char= other #\-))
           (and (char= one #\-)
                (char= other #\+))
           (and (char= one #\+)
                (char= other #\-)))))

(defun not-parallel-horizontal (one other)
  (not (or (and (char= one #\|)
                (char= other #\|))
           (and (char= one #\|)
                (char= other #\+))
           (and (char= one #\+)
                (char= other #\|)))))

(defun add-above (row col cell-entry grid graph)
  (let ((above (get-from-grid (- row 1) col grid)))
    (if (and (char/= above #\space)
             (not-parallel-vertical above cell-entry))
        (push (cons (- row 1) col) 
              (gethash (cons row col) graph)))))

(defun add-below (row col cell-entry grid graph)
  (let ((below (get-from-grid (+ row 1) col grid)))
    (if (and (char/= below #\space)
             (not-parallel-vertical below cell-entry))
        (push (cons (+ row 1) col)
              (gethash (cons row col) graph)))))

(defun add-right (row col cell-entry grid graph)
  (let ((right (get-from-grid row (+ col 1) grid)))
    (if (and (char/= right #\space)
             (not-parallel-horizontal right cell-entry))
        (push (cons row (+ col 1))
              (gethash (cons row col) graph)))))

(defun add-left (row col cell-entry grid graph)
  (let ((left (get-from-grid row (- col 1) grid)))
    (if (and (char/= left #\space)
             (not-parallel-horizontal left cell-entry))
        (push (cons row (- col 1))
              (gethash (cons row col) graph)))))

(defun add-neighbours (row col grid graph)
  (let ((cell-entry (get-from-grid row col grid)))
    (if (char/= cell-entry #\space)
        (progn
          (if (> row 0) (add-above row col cell-entry grid graph))
          (if (> col 0) (add-left row col cell-entry grid graph))
          (if (< row (- (length grid) 1)) (add-below row col cell-entry grid graph))
          (if (< col (- (length (aref grid 0)) 1)) (add-right row col cell-entry grid graph))))))

(defun process-cells (grid graph)
  (loop for row from 0 to (- (length grid) 1)
     do (loop for col from 0 to (- (length (aref grid 0)) 1)
           do (add-neighbours row col grid graph))))

(defun build-grid (filename)
  (with-open-file (f filename)
    (when f
      (map 'vector #'identity (loop for line = (read-line f nil nil)
                      while line collect line)))))

(defun build-graph (grid)
  (let ((graph (make-hash-table :test 'equal)))
    (process-cells grid graph)
    graph))

(defun find-start (grid)
  (loop for i from 0 to (- (length (aref grid 0)) 1)       
       do (progn
            (if (char/= (get-from-grid 0 i grid) #\space)
                (return-from find-start (cons 0 i))))))

(defun is-neighbour (cell row col graph)
  (find cell (gethash (cons row col) graph) :test 'equal))

(defun move-down (row col graph)
  (if (is-neighbour (cons (+ row 1) col) row col graph)
      (values (+ row 1) col 'down)
      (if (is-neighbour (cons row (+ col 1)) row col graph)
          (values row (+ col 1) 'right)
          (if (is-neighbour (cons row (- col 1)) row col graph)
              (values row (- col 1) 'left)
              nil))))

(defun move-up (row col graph)
  (if (is-neighbour (cons (- row 1) col) row col graph)
      (values (- row 1) col 'up)
      (if (is-neighbour (cons row (+ col 1)) row col graph)
          (values row (+ col 1) 'right)
          (if (is-neighbour (cons row (- col 1)) row col graph)
              (values row (- col 1) 'left)
              nil))))

(defun move-right (row col graph)
  (if (is-neighbour (cons row (+ col 1)) row col graph)
      (values row (+ col 1) 'right)
      (if (is-neighbour (cons (+ row 1) col) row col graph)
          (values (+ row 1) col 'down)
          (if (is-neighbour (cons (- row 1) col) row col graph)
              (values (- row 1) col 'up)
              nil))))

(defun move-left (row col graph)
  (if (is-neighbour (cons row (- col 1)) row col graph)
      (values row (- col 1) 'left)
      (if (is-neighbour (cons (+ row 1) col) row col graph)
          (values (+ row 1) col 'down)
          (if (is-neighbour (cons (- row 1) col) row col graph)
              (values (- row 1) col 'up)
              nil))))

(defun move (row col graph direction)
  (case direction
    (up (move-up row col graph))
    (down (move-down row col graph))
    (right (move-right row col graph))
    (left (move-left row col graph))))

(defun navigate (graph start grid)
  (let (seen (steps 0))
    (labels ((recur (row col direction)
               (let ((entry (get-from-grid row col grid)))
                 (incf steps)
                 (if (alpha-char-p entry)
                     (push entry seen))
                 (multiple-value-bind (next-row next-col next-direction)
                   (move row col graph direction)
                   (if next-row
                       (recur next-row next-col next-direction)
                       (values (map 'string #'identity (reverse seen)) steps))))))
      (recur (car start) (cdr start) 'down))))

(defun part-1-solver (filename)
  (let ((grid (build-grid filename)))
    (let ((graph (build-graph grid))
          (start (find-start grid)))
      (navigate graph start grid))))

(defun test-part-1 ()
  (part-1-solver "series-of-tubes-test-input"))

(defun solution-part-1 ()
  (part-1-solver "series-of-tubes-input.txt"))