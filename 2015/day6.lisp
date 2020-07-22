(ql:quickload :cl-ppcre)

(defun get-coords (line)
  (multiple-value-bind (match coords)
      (cl-ppcre:scan-to-strings "[^\\d]*(\\d+),(\\d+)[^\\d]*(\\d+),(\\d+)[^\\d]*" line)
    (declare (ignorable match))
    (map 'list #'parse-integer coords)))

(defun toggle-part-1 (command blocks index)
  (declare (optimize (speed 3))
           ((simple-array fixnum) blocks)
           (fixnum index))
  (case command
    (turn-on (setf (aref blocks index) 1))
    (turn-off (setf (aref blocks index) 0))
    (toggle (setf (aref blocks index) 
                  (logxor 1 (aref blocks index))))))

(defun toggle-lights (command blocks r1 c1 r2 c2 f)
  (declare (optimize (speed 3))
           ((simple-array fixnum) blocks)
           ((unsigned-byte 16) r1 r2 c1 c2)
           (function f))
  (loop for r fixnum from r1 to r2
     do (loop for c fixnum from c1 to c2
           for index = (+ (* r 1000) c)
           do (funcall f command blocks index))))

(defun switch-lights (lines f)
  (let ((blocks (make-array 1000000 :element-type 'fixnum)))
    (declare ((simple-array fixnum) blocks) 
             (function f))
    (loop for line in lines
       for (r1 c1 r2 c2) = (get-coords line)
       do (cond ((search "turn on" line) (toggle-lights 'turn-on blocks r1 c1 r2 c2 f))
                ((search "turn off" line) (toggle-lights 'turn-off blocks r1 c1 r2 c2 f))
                ((search "toggle" line) (toggle-lights 'toggle blocks r1 c1 r2 c2 f))))
    blocks))

(defun part-1 ()
  (declare (optimize (speed 3)))
  (let ((lines (with-open-file (f "input6") 
                 (when f
                   (loop for line = (read-line f nil nil)
                      while line collect line)))))
    (count-if (lambda (x) (declare (fixnum x)) (= x 1)) (switch-lights lines #'toggle-part-1))))

(defun toggle-part-2 (command blocks index)
  (declare (optimize (speed 3))
           ((simple-array fixnum) blocks)
           (fixnum index))
  (case command 
    (turn-on (incf (aref blocks index)))
    (turn-off (setf (aref blocks index) (max 0 (- (aref blocks index) 1))))
    (toggle (incf (aref blocks index) 2))))

(defun part-2 ()
  (declare (optimize (speed 3)))
  (let ((lines (with-open-file (f "input6")
                 (when f
                   (loop for line = (read-line f nil nil)
                      while line collect line)))))
    (reduce #'+ (switch-lights lines #'toggle-part-2))))
