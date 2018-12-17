(load "utilities.lisp")
(ql:quickload :cl-ppcre)

(defun up (pos)
  (cons (car pos) (1- (cdr pos))))

(defun down (pos)
  (cons (car pos) (1+ (cdr pos))))

(defun left (pos)
  (cons (1- (car pos)) (cdr pos)))

(defun right (pos)
  (cons (1+ (car pos)) (cdr pos)))

(defparameter *input-filename* "reservoir-research-input.txt")
(defparameter *test-input-filename* "reservoir-research-test-input.txt")

(defun parse-clay-vein (line)
  (multiple-value-bind (m rs) 
      (cl-ppcre:scan-to-strings "(\\w)=(\\d+),\\s+(\\w)=(\\d+)..(\\d+)" line)
    (declare (ignore m))
    (list (intern (string-upcase (aref rs 0)))
          (parse-integer (aref rs 1))
          (intern (string-upcase (aref rs 2)))
          (parse-integer (aref rs 3))
          (parse-integer (aref rs 4)))))

(defun read-veins (filename)
  (read-lines filename #'parse-clay-vein))

(defun find-maximum-depth (clay)
  (loop for block being the hash-keys of clay
     maximize (cdr block)))

(defun set-clay-vein (vein clay)
  (let ((start-x 0) (start-y 0) (end-x 0) (end-y 0))
    (destructuring-bind (one start-one other start-other end-other) vein
      (declare (ignore other))
      (case one
        (x (setf start-x start-one
                 end-x start-one
                 start-y start-other
                 end-y end-other))
        (y (setf start-x start-other
                 end-x end-other
                 start-y start-one
                 end-y start-one))))
    (loop for y from start-y to end-y
       do (loop for x from start-x to end-x
             do (setf (gethash (cons x y) clay) t)))))

(defun set-clay-veins (veins)
  (loop for vein in veins
     with clay = (make-hash-table :test 'equal)
     do (set-clay-vein vein clay)
     finally (return clay)))

(defun move-water (position clay visited filled max-depth)
  (when (<= (cdr position) max-depth)
    (format t "MOVE WATER ~a~%" position)
    (setf (gethash position visited) t))
  (labels ((fill-position (position)
;;             (format t "FILLING ~a~%" position)
             (setf (gethash position filled) t)
             (when (and (not (gethash (left position) filled))
                        (not (gethash (left position) clay)))
               (fill-position (left position)))
             (when (and (not (gethash (right position) filled))
                        (not (gethash (right position) clay)))
               (fill-position (right position)))
             (when (and (not (gethash (down position) filled))
                        (not (gethash (down position) clay)))
               (fill-position (down position)))))
    (cond ((> (cdr position) max-depth) t)
          ((and (gethash (down position) visited) (not (gethash (down position) filled)))
           t)
          ((and (not (gethash (down position) clay)) (not (gethash (down position) filled)))
           (let ((leaked-down (move-water (down position) clay visited filled max-depth)))
             (if (not leaked-down)
                 (let ((leaked-left (and (not (gethash (left position) clay))
                                         (move-water (left position) 
                                                     clay visited filled max-depth)))
                       (leaked-right (and (not (gethash (right position) clay))
                                          (move-water (right position)
                                                      clay visited filled max-depth))))
                   (when (not (or leaked-left leaked-right))
                     (fill-position position))
                   (or leaked-left leaked-right))
                 t)))
          (t (let ((alone (and (not (gethash (left position) visited))
                               (not (gethash (right position) visited))))
                   (leaked-left (and (not (gethash (left position) clay))
                                     (not (gethash (left position) visited))
                                     (move-water (left position) clay visited filled max-depth)))
                   (leaked-right (and (not (gethash (right position) clay))
                                      (not (gethash (right position) visited))
                                      (move-water (right position)
                                                  clay visited filled max-depth))))
               (when (and alone (not (or leaked-left leaked-right)))
                 (fill-position position))
               (or leaked-right leaked-left))))))

(defun min-x (clay)
  (loop for block being the hash-keys of clay
     minimize (car block)))

(defun bounding-box (clay)
  (loop for block being the hash-keys of clay
     minimize (car block) into min-x
     maximize (car block) into max-x
     maximize (cdr block) into max-y
     finally (return (list min-x max-x 0 max-y))))

(defun print-clay (clay visited filled)
  (destructuring-bind (min-x max-x min-y max-y) (bounding-box clay)
    (format t "TOP LEFT ~a~%" (cons min-x min-y))
    (loop for y from min-y to max-y
       do (loop for x from min-x to max-x
             do (cond ((gethash (cons x y) filled)
                       (format t "~~"))
                      ((gethash (cons x y) visited)
                       (format t "|"))
                      ((gethash (cons x y) clay)
                       (format t "#"))                     
                      (t (format t "."))))
         (format t "~%"))))

(defun test-1 ()
  (let* ((veins (read-veins *test-input-filename*))
         (clay (set-clay-veins veins))
         (visited (make-hash-table :test 'equal))
         (filled (make-hash-table :test 'equal))
         (max-depth (find-maximum-depth clay)))
    (move-water (cons 500 0) clay visited filled max-depth)
    (print-clay clay visited filled)
    (1- (hash-table-count visited))))

(defun solution-part-1 ()
  (let* ((veins (read-veins *input-filename*))
         (clay (set-clay-veins veins))
         (visited (make-hash-table :test 'equal))
         (filled (make-hash-table :test 'equal))
         (max-depth (find-maximum-depth clay)))
    (move-water (cons 500 0) clay visited filled max-depth)
    (print-clay clay visited filled)
    (1- (hash-table-count visited))))
