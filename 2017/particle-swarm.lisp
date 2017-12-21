(ql:quickload 'cl-ppcre)

(defun parse-particle (line)
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings "[^\\d-]+(-?\\d+),(-?\\d+),(-?\\d+)[^\\d-]+(-?\\d+),(-?\\d+),(-?\\d+)[^\\d-]+(-?\\d+),(-?\\d+),(-?\\d+)[^\\d]+" line)
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

(defun update-particle (particle)
  (destructuring-bind (px py pz vx vy vz ax ay az) particle
    (let ((vx+ (+ vx ax))
          (vy+ (+ vy ay))
          (vz+ (+ vz az)))
      (list (+ px vx+) (+ py vy+) (+ pz vz+)
            vx+ vy+ vz+
            ax ay az))))

(defun update-positions (particles)
  (if (null particles)
      nil
      (cons (update-particle (car particles))
            (update-positions (cdr particles)))))

(defun take-3 (particle)
  (list (car particle) (cadr particle) (caddr particle)))

(defun has-collisions (particles)
  (let ((positions (make-hash-table :test 'equal)))
    (format t "~a~%" particles)
    (loop for p in particles
       do (let ((key (take-3 p)))
            (format t "~a~%" key)
            (if (gethash key positions)
                (return-from has-collisions t)
                (setf (gethash key positions) t))))
    nil))

(defun remove-collisions (particles)
  (let ((positions (make-hash-table :test 'equal)))
    (loop for p in particles
       do (let ((key (take-3 p)))
            (if (gethash key positions)
                (incf (gethash key positions))
                (setf (gethash key positions) 1))))
    (remove-if (lambda (p) (> (gethash (take-3 p) positions) 1)) particles)))

(defun loop-until-collision (particles)
  (loop until (has-collisions particles)
     do (setf particles (update-positions particles)))
  particles)

(defun test-part-2 ()
  (loop-until-collision (test-points)))

(defun first-collision-real-points ()
  (loop-until-collision (real-points)))

(defun loop-and-remove-collisions (particles iterations)
  (loop for i from 1 to iterations
     do (setf particles (remove-collisions (update-positions particles))))
  particles)

(defun loop-and-remove-test (iterations)
  (loop-and-remove-collisions (test-points) iterations))

(defun loop-and-remove-real (iterations)
  (loop-and-remove-collisions (real-points) iterations))

(defun test-points ()
  (collect-points "particles-test-input-2"))

(defun real-points ()
  (collect-points "particle-swarm-input.txt"))

(defun part-1-solver (particles)
  (srt-pnts particles))

(defun test-part-1 ()
  (car (part-1-solver (test-points))))

(defun solution-part-1 ()
  (car (part-1-solver (real-points))))
