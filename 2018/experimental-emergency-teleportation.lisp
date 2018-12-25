(load "utilities.lisp")
(load "priority-queue.lisp")
(ql:quickload :cl-ppcre)

(defparameter *test-input-file* "experimental-emergency-teleportation-test-input.txt")
(defparameter *input-file* "experimental-emergency-teleportation-input.txt")

(defun read-nano-bot (line)
  (multiple-value-bind (m rs)
      (cl-ppcre:scan-to-strings ".*<(-?\\d+),(-?\\d+),(-?\\d+)>,\\s+r=(\\d+)" line)
    (declare (ignore m))
    (map 'vector #'parse-integer rs)))

(defun read-input (filename)
  (coerce (read-lines filename #'read-nano-bot) 'vector))

(defun signal-strength (bot)
  (aref bot 3))

(defun find-strongest-bot (bots)
  (loop with best = nil
     with strength = 0
     for bot across bots
     when (> (signal-strength bot) strength)
     do (setf best bot
              strength (signal-strength bot))
     finally (return best)))

(defun manhattan-distance (one other)
  (+ (abs (- (aref one 0) (aref other 0)))
     (abs (- (aref one 1) (aref other 1)))
     (abs (- (aref one 2) (aref other 2)))))

(defun in-range (one)
  (lambda (other) 
    (<= (manhattan-distance one other) (signal-strength one))))

(defun solution-part-1 ()
  (let* ((bots (read-input *input-file*))
         (strongest (find-strongest-bot bots)))
    (length (remove-if-not (in-range strongest) bots))))

(defun find-bounding-box (nano-bots)
  (loop for bot across nano-bots 
     minimize (aref bot 0) into min-x
     minimize (aref bot 1) into min-y
     minimize (aref bot 2) into min-z
     maximize (aref bot 0) into max-x
     maximize (aref bot 1) into max-y
     maximize (aref bot 2) into max-z
     finally (return (vector min-x max-x min-y max-y min-z max-z))))

(defun equal-bots (one other)
  (every #'= one other))

(defun min-x (bounding-box)
  (aref bounding-box 0))

(defun max-x (bounding-box)
  (aref bounding-box 1))

(defun min-y (bounding-box)
  (aref bounding-box 2))

(defun max-y (bounding-box)
  (aref bounding-box 3))

(defun min-z (bounding-box)
  (aref bounding-box 4))

(defun max-z (bounding-box)
  (aref bounding-box 5))

(defun x (bot) (aref bot 0))
(defun y (bot) (aref bot 1))
(defun z (bot) (aref bot 2))

(defun inside-bounding-box (bot bounding-box)
  (and (<= (min-x bounding-box) (x bot) (max-x bounding-box))
       (<= (min-y bounding-box) (y bot) (max-y bounding-box))
       (<= (min-z bounding-box) (z bot) (max-z bounding-box))))

(defun distance-to-bbox (bot bounding-box)
  (+ (if (<= (min-x bounding-box) (x bot) (max-x bounding-box)) 0 
         (min (abs (- (x bot) (min-x bounding-box))) (abs (- (x bot) (max-x bounding-box)))))
     (if (<= (min-y bounding-box) (y bot) (max-y bounding-box)) 0
         (min (abs (- (y bot) (min-y bounding-box))) (abs (- (y bot) (max-y bounding-box)))))
     (if (<= (min-z bounding-box) (z bot) (max-z bounding-box)) 0
         (min (abs (- (z bot) (min-z bounding-box))) (abs (- (z bot) (max-z bounding-box)))))))

(defun bot-in-range (bot bounding-box)
  (<= (distance-to-bbox bot bounding-box) (signal-strength bot)))

(defun make-space (bounding-box bots)
  (let* ((intersections (count-if (lambda (bot) (bot-in-range bot bounding-box)) bots))
         (size (+ (- (max-x bounding-box) (min-x bounding-box))
                  (- (max-y bounding-box) (min-y bounding-box))
                  (- (max-z bounding-box) (min-z bounding-box))))
         (distance-to-origin (+ (abs (min-x bounding-box)) 
                                (abs (min-y bounding-box)) 
                                (abs (min-z bounding-box)))))
    (list intersections size bounding-box distance-to-origin)))

(defun size (space) (cadr space))
(defun intersections (space) (car space))
(defun get-bounding-box (space) (caddr space))
(defun distance-to-origin (space) (cadddr space))

(defun space-ordering (one other)
  (or (> (intersections one)
         (intersections other))
      (and (= (intersections one)
              (intersections other))
           (or (< (distance-to-origin one) (distance-to-origin other))
               (and (= (distance-to-origin one) (distance-to-origin other))
                    (< (size one) (size other)))))))

(defun split-bounding-box (bounding-box)
  (let ((mid-x (floor (+ (min-x bounding-box) (max-x bounding-box)) 2))
        (mid-y (floor (+ (min-y bounding-box) (max-y bounding-box)) 2))
        (mid-z (floor (+ (min-z bounding-box) (max-z bounding-box)) 2)))
    (vector (vector (min-x bounding-box) mid-x
                    (min-y bounding-box) mid-y
                    (min-z bounding-box) mid-z)
            (vector (min-x bounding-box) mid-x
                    (min-y bounding-box) mid-y
                    mid-z (max-z bounding-box))
            (vector (min-x bounding-box) mid-x
                    mid-y (max-y bounding-box)
                    (min-z bounding-box) mid-z)
            (vector (min-x bounding-box) mid-x
                    mid-y (max-y bounding-box)
                    mid-z (max-z bounding-box))
            (vector mid-x (max-x bounding-box)
                    (min-y bounding-box) mid-y
                    (min-z bounding-box) mid-z)
            (vector mid-x (max-x bounding-box)
                    (min-y bounding-box) mid-y
                    mid-z (max-z bounding-box))
            (vector mid-x (max-x bounding-box)
                    mid-y (max-y bounding-box)
                    (min-z bounding-box) mid-z)
            (vector mid-x (max-x bounding-box)
                    mid-y (max-y bounding-box)
                    mid-z (max-z bounding-box)))))

(defun partition (bounding-box bots)
  (let ((sub-spaces (split-bounding-box bounding-box)))
    (map 'vector (lambda (sub-space) (make-space sub-space bots)) sub-spaces)))

(defun enqueue-subspaces (space q bots)
  (loop with sub-spaces = (let ((spaces (partition (get-bounding-box space) bots)))
                            spaces)
     for sp across sub-spaces
      do (insert-pq sp q)))

(defun find-best-coordinate (bounding-box bots)
  (let ((q (make-pq #'space-ordering)))
    (insert-pq (make-space bounding-box bots) q)
    (loop with seen = (make-hash-table :test 'equal)
       for space = (pop-pq q)
       for key = (map 'list #'identity (get-bounding-box space))
       while (and space (> (size space) 0))
       when (not (gethash key seen))       
       do (enqueue-subspaces space q bots)
         (setf (gethash key seen) t)
       finally (return space))))

(defun test-2 ()
  (let* ((bots (read-input *test-input-file*))
         (bounding-box (find-bounding-box bots)))
    (find-best-coordinate bounding-box bots)))

(defun solution-part-2 ()
  (let* ((bots (read-input *input-file*))
         (bounding-box (find-bounding-box bots)))
    (find-best-coordinate bounding-box bots)))

(defun find-bots-in-range (point bots)
  (loop for bot across bots 
     count (<= (manhattan-distance bot point) (signal-strength bot))))
