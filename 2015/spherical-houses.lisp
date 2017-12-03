(defun move (location direction)
  (case direction
    (#\^ (cons (+ (car location) 1) (cdr location)))
    (#\v (cons (- (car location) 1) (cdr location)))
    (#\> (cons (car location) (+ (cdr location) 1)))
    (#\< (cons (car location) (- (cdr location) 1)))))

(defun navigate (location instructions callback)
  (if (null instructions)
      location
      (progn (navigate (funcall callback location (car instructions))
                       (cdr instructions)
                       callback))))

(defun houses-visited (instructions)
  (let ((visited (make-hash-table :test 'equal))
        (total 0))
    (setf (gethash '(0 . 0) visited) t)
    (navigate '(0 . 0) instructions 
              (lambda (location instruction)
                (setf (gethash location visited) t)
                (move location instruction)))
    (loop for location being the hash-keys of visited
       do (incf total))
    total))

(defun robo-and-santa-visited (instructions)
  (let ((visited (make-hash-table :test 'equal))
        (total 0)
        (santas-turn t)
        (robo-location '(0 . 0))
        (santa-location '(0 . 0)))
    (setf (gethash '(0 . 0) visited) t)
    (navigate '(0 . 0) instructions
              (lambda (location instruction)
                (let ((new-location (move location instruction)))
                  (setf (gethash new-location visited) t)
                  (if santas-turn
                      (progn (setf santas-turn nil)
                             (setf santa-location new-location)
                             robo-location)
                      (progn (setf santas-turn t)
                             (setf robo-location new-location)
                             santa-location)))))
    (loop for location being the hash-keys of visited
       do (incf total))
    total))

(defun get-input (filename)
  (with-open-file (f filename)
    (when f
      (coerce (read-line f nil nil) 'list))))

(defun solution-part-1 ()
  (let ((instructions (get-input "spherical-houses-input.txt")))
    (houses-visited instructions)))

(defun solution-part-2 ()
  (let ((instructions (get-input "spherical-houses-input.txt")))
    (robo-and-santa-visited instructions)))
