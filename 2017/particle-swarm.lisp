(ql:quickload 'cl-ppcre)

(defun parse-particle (line)
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings "[^\\d]+(-?\\d+),(-?\\d+),(-?\\d+)[^\\d]+(-?\\d+),(-?\\d+),(-?\\d+)[^\\d]+(-?\\d+),(-?\\d+),(-?\\d+)[^\\d]+" line)
    (declare (ignorable match))
    (map 'list #'parse-integer registers)))

(defun cmp-val (particle) 
  (destructuring-bind (px py pz vx vy vz ax ay az) particle
    (list (+ (abs ax) (abs ay) (abs az))
          (+ (abs vx) (abs vy) (abs vz))
          (+ (abs px) (abs py) (abs pz)))))

(defun compare (a b table)
  (let ((a-cmp (gethash a table))
        (b-cmp (gethash b table)))
    (or (< (car a-cmp) (car b-cmp))
        (and (= (car a-cmp) (car b-cmp))
             (< (cadr a-cmp) (cadr b-cmp)))
        (and (= (cadr a-cmp) (cadr b-cmp))
             (< (caddr a-cmp) (caddr b-cmp))))))

(defun srt-pnts (particles)
  (let ((cmps (mapcar #'cmp-val particles))
        (table (make-hash-table))
        (i 0))
    (let ((indices (loop for cmp in cmps
                      collect i
                      do (progn (setf (gethash i table) cmp)
                                (incf i)))))
      (sort indices (lambda (a b) (compare a b table))))))

(defun collect-points (filename)
  (with-open-file (f filename)
    (when f
      (loop for line = (read-line f nil nil)
         while line collect (parse-particle line)))))

(defun test-points ()
  (collect-points "particle-swarm-test-input"))

(defun real-points ()
  (collect-points "particle-swarm-input.txt"))

(defun part-1-solver (particles)
  (srt-pnts particles))

(defun test-part-1 ()
  (car (part-1-solver (test-points))))

(defun solution-part-1 ()
  (car (part-1-solver (real-points))))
