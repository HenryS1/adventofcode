(load "utilities.lisp")
(ql:quickload :cl-ppcre)

(defun read-star (line)
  (multiple-value-bind (m rs) (cl-ppcre:scan-to-strings 
                              ".*< ?(-?\\d+),  ?(-?\\d+)>.*< ?(-?\\d+),  ?(-?\\d+)" line)
    (declare (ignore m))
    (map 'list #'parse-integer rs)))

(defun read-stars ()
  (make-table (read-lines "the-stars-align-input.txt" #'read-star) 
              (lambda (star) (cons (car star) (cadr star)))))

(defmacro x (star) 
  `(car ,star))

(defmacro y (star)
  `(cadr ,star))

(defmacro vx (star)
  `(caddr ,star))

(defmacro vy (star)
  `(cadddr ,star))

(defun move-star (star)
  (incf (x star) (vx star))
  (incf (y star) (vy star))
  star)

(defun move-stars (stars)
  (let ((new-stars (make-hash-table :test 'equal)))
    (maphash (lambda (k star) 
               (move-star star)
               (setf (car k) (car star))
               (setf (cdr k) (cadr star))
               (setf (gethash k new-stars) star))
             stars) 
    new-stars))

(defun min-x (stars)
  (reduce #'min (mapcar #'car stars)))

(defun max-x (stars)
  (reduce #'max (mapcar #'car stars)))

(defun min-y (stars)
  (reduce #'min (mapcar #'cadr stars)))

(defun max-y (stars)
  (reduce #'max (mapcar #'cadr stars)))

(defun bounding-box (stars)
  (list (min-x stars) (min-y stars) (max-x stars) (max-y stars)))

(defun height (stars)
  (destructuring-bind (mnx mny mxx mxy) (bounding-box stars)
    (declare (ignore mnx mxx))
    (- mxy mny)))

(defun watch-stars (stars)
  (loop for i from 1 to 100000
     for tm = 0 then (1+ tm)
     do (when (< (height (hash-values stars)) 10)
          (show-stars stars)
          (return-from watch-stars tm))
       (setf stars (move-stars stars))))

(defun show-stars (stars)
  (destructuring-bind (mnx mny mxx mxy) (bounding-box (hash-values stars))
    (loop for y from mny to mxy
       do (loop for x from mnx to mxx
             do (if (gethash (cons x y) stars)
                    (format t "#")
                    (format t "."))
             finally (format t "~%"))
       finally (format t "~%"))))

