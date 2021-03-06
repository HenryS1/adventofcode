(defun cell-power (coord serial-number)
  (destructuring-bind (x . y) coord
    (let* ((rack-id (+ x 10))
           (power-level (* rack-id y)))
      (incf power-level serial-number)
      (setf power-level (* power-level rack-id))
      (setf power-level (mod (floor power-level 100) 10))
      (decf power-level 5))))

(defun construct-grid ()
  (map 'vector #'identity (loop for y from 1 to 300
                             collect (map 'vector #'identity (loop for x from 1 to 300
                                                                collect (cons x y))))))

(defun compute-power-levels (grid serial-number)
  (map 'vector (lambda (vec) 
                 (map 'vector (lambda (coord) (cell-power coord serial-number)) vec))
       grid))

(defun three-by-three-power (top-left-coord grid)
  (let ((power 0))
    (destructuring-bind (cx . cy) top-left-coord
      (loop for y from cy to (+ cy 2)
         do (loop for x from cx to (+ cx 2)
               do (incf power (aref (aref grid (1- y)) (1- x))))))
    power))

(defun find-maximum-power-square (grid)
  (let ((mx 0)
        best)
    (loop for x from 1 to (- 300 2)
       do (loop for y from 1 to (- 300 2)
             do (let ((power (three-by-three-power (cons x y) grid)))
                  (when (> power mx)
                    (setf mx power)
                    (setf best (cons x y))))))
    (values best mx)))

(defun solution-part-1 ()
  (find-maximum-power-square (compute-power-levels (construct-grid) 7403)))

(defun cumulative-sum-square (grid)
  (loop for x from 1 to 300
     do (loop for y from 1 to 300
           do (incf (aref (aref grid (1- y)) (1- x)) 
                    (if (> y 1)
                        (aref (aref grid (- y 2)) (1- x))
                        0))
             (incf (aref (aref grid (1- y)) (1- x))
                   (if (> x 1)
                       (aref (aref grid (1- y)) (- x 2))
                       0))
           when (and (> x 1) (> y 1))
           do (decf (aref (aref grid (1- y)) (1- x))
                    (aref (aref grid (- y 2)) (- x 2)))))
  grid)

(defun find-square-power (size coord grid)
  (destructuring-bind (cx . cy) coord
    (let ((left-offset (+ cx size -1))
          (top-offset (+ cy size -1)))
      (+ (- (aref (aref grid (1- top-offset)) (1- left-offset))
            (if (> cy 1) 
                (aref (aref grid (- cy 2)) (1- left-offset))
                0)
            (if (> cx 1)
                (aref (aref grid (1- top-offset)) (- cx 2))
                0))
         (if (and (> cy 1) (> cx 1))
             (aref (aref grid (- cy 2)) (- cx 2))
             0)))))

(defun find-max-square-of-size (size grid)
  (let ((mx -99999999)
        best)
    (loop for y from 1 to (- 300 size)
       do (loop for x from 1 to (- 300 size)
             do (let ((square-power (find-square-power size (cons x y) grid)))
                  (when (> square-power mx)
                    (setf mx square-power)
                    (setf best (cons x y))))))
    (values best mx)))

(defun find-max-square-of-any-size (grid)
  (let ((mx -99999999)
        (best-size 0)
        best-coord)
    (loop for size from 1 to 300
       do (multiple-value-bind (coord max-power) (find-max-square-of-size size grid)
            (when (> max-power mx)
              (setf mx max-power)
              (setf best-size size)
              (setf best-coord coord))))
    (values best-coord best-size mx)))

(defun solution-part-2 ()
  (let ((grid (cumulative-sum-square (compute-power-levels (construct-grid) 7403))))
    (find-max-square-of-any-size grid)))
