(ql:quickload :cl-ppcre)
(load "utilities.lisp")

(defun find-bounding-box (points)
  (let ((min-row (reduce #'min (mapcar #'car points)))
        (max-row (reduce #'max (mapcar #'car points)))
        (min-col (reduce #'min (mapcar #'cadr points)))
        (max-col (reduce #'max (mapcar #'cadr points))))
    (list min-row max-row min-col max-col)))

(defun manhattan-distance (one other)
  (+ (abs (- (car one) (car other)))
     (abs (- (cadr one) (cadr other)))))

(defun find-closest-point (point points)
  (let ((mn 9999999)
        best)
    (loop for p in points
       do (when (= (manhattan-distance p point) mn)
           (push p best))
         (when (< (manhattan-distance p point) mn)
            (setf best (list p))
            (setf mn (manhattan-distance p point))))
    (when (null (cdr best))
        (car best))))

(defun assign-closest-points (bounding-box points)
  (let ((assignments (make-hash-table :test 'equal)))
    (destructuring-bind (min-row max-row min-col max-col) bounding-box
      (loop for row from min-row to max-row
         do (loop for col from min-col to max-col
               do (let ((closest-point (find-closest-point (list row col) points)))
                    (push (list row col) (gethash closest-point assignments))))))
    assignments))

(defun on-boundary (area bounding-box)
  (loop for point in area
     when (or (= (car point) (car bounding-box)) 
              (= (car point) (cadr bounding-box))
              (= (cadr point) (caddr bounding-box))
              (= (cadr point) (cadddr bounding-box)))
     return t))

(defun remove-infinite-areas (assignments bounding-box)
  (filter-hash-values assignments (lambda (area) (not (on-boundary area bounding-box)))))

(defun find-max-area (assignments)
  (let ((mx 0)
        best-point)
    (maphash (lambda (point area) 
               (when (> (length area) mx)
                 (setf mx (length area))
                 (setf best-point point))) 
             assignments)
    (cons best-point mx)))

(defun parse-point (line)
  (mapcar #'parse-integer (cl-ppcre:split ",\\s+" line)))

(defun solution-part-1 ()
  (let ((points (read-lines "chronal-coordinates-input.txt" #'parse-point)))
    (let* ((bounding-box (find-bounding-box points))
           (assignments (remove-infinite-areas (assign-closest-points bounding-box points) 
                                               bounding-box)))
      (find-max-area assignments))))

(defun close-to-all (point points distance)
  (let ((total-distance (reduce #'+ (mapcar (lambda (p) (manhattan-distance p point)) points))))
    (< total-distance distance)))

(defun count-close-coordinates (bounding-box points distance)
  (let ((count 0))
    (destructuring-bind (min-row max-row min-col max-col) bounding-box
      (loop for row from min-row to max-row 
         do (loop for col from min-col to max-col
               when (close-to-all (list row col) points distance)
               do (incf count))))
    count))

(defun solution-part-2 ()
  (let ((points (read-lines "chronal-coordinates-input.txt" #'parse-point)))
    (let ((bounding-box (find-bounding-box points)))
      (count-close-coordinates bounding-box points 10000))))
