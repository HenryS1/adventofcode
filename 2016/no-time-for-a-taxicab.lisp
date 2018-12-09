(load "../2018/utilities.lisp")
(ql:quickload :cl-ppcre)

(defun read-input ()
  (car (with-open-file (f "no-time-for-a-taxicab-input.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line
         collect (parse-instructions line))))))

(defun parse-instructions (line)
  (let ((cleaned (cl-ppcre:split ",?\\s+" line)))
    (loop for direction in cleaned
       collect (if (char= (aref direction 0) #\R)
                   (cons 'R (parse-integer (subseq direction 1)))
                   (cons 'L (parse-integer (subseq direction 1)))))))

(defun turn-right (direction)
  (case direction
    (N 'E)
    (E 'S)
    (S 'W)
    (W 'N)))

(defun turn-left (direction)
  (case direction
    (N 'W)
    (E 'N)
    (S 'E)
    (W 'S)))

(defun turn (current-direction turn-direction)
  (case turn-direction
    (L (turn-left current-direction))
    (R (turn-right current-direction))))

(defun take-steps (position direction steps)
  (case direction
    (N (cons (+ (car position) steps) (cdr position)))
    (S (cons (- (car position) steps) (cdr position)))
    (E (cons (car position) (+ (cdr position) steps)))
    (W (cons (car position) (- (cdr position) steps)))))

(defun move (position current-direction instruction)
  (setf current-direction (turn current-direction (car instruction))) 
  (values (take-steps position current-direction (cdr instruction)) current-direction))

(defun move-to-end (position direction instructions)
  (loop for instruction in instructions 
       do (multiple-value-bind (new-position new-direction) (move position direction instruction)
            (setf direction new-direction)
            (setf position new-position)))
  position)

(defun solution-part-1 ()
  (let ((instructions (read-input))
        (position (cons 0 0))
        (direction 'N))
    (setf position (move-to-end position direction instructions))
    (+ (abs (car position)) (abs (cdr position)))))

(defun find-first-visited-twice (instructions)
  (let ((position (cons 0 0))
        (direction 'N)
        (seen (make-hash-table :test 'equal)))
    (setf (gethash position seen) t)
    (loop for instruction in instructions 
       do (multiple-value-bind (new-position new-direction) (move position direction instruction)
            (remhash position seen)
            (destructuring-bind ((s-row . s-col) (e-row . e-col)) (list position new-position)
              (if (or (> s-row e-row) (> s-col e-col))
                  (loop for row from s-row downto e-row
                     do (loop for col from s-col downto e-col
                           do (let ((p (cons row col)))
                                (when (gethash p seen)
                                  (return-from find-first-visited-twice p))
                                (setf (gethash p seen) t))))
                  (loop for row from s-row to e-row
                     do (loop for col from s-col to e-col
                           do (let ((p (cons row col)))
                                (when (gethash p seen)
                                  (return-from find-first-visited-twice p))
                                (setf (gethash p seen) t))))))
            (setf direction new-direction)
            (setf position new-position)))))

(defun solution-part-2 ()
  (let ((instructions (read-input)))
    (find-first-visited-twice instructions)))
