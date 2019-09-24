(defun is-a-trap (left centre right)
  (or (and left centre (not right))
      (and centre right (not left))
      (and left (not centre) (not right))
      (and right (not centre) (not left))))

(defun find-next-row (row)
  (loop with new-row = (make-string (length row))
     for i from 0 to (- (length row) 1)
     for left = nil then (char= (aref row (- i 1)) #\^)
     for centre = (char= (aref row i) #\^)
     for right = (if (< i (- (length row) 1))
                     (char= (aref row (+ i 1)) #\^)
                     nil)
     if (is-a-trap left centre right)
     do (setf (aref new-row i) #\^)
     else do (setf (aref new-row i) #\.)
     finally (return new-row)))

(defun count-safe-tiles (start-row rows)
  (loop for i from 1 to rows
     for row = start-row then (find-next-row row)
     sum (count-if (lambda (c) (char= c #\.)) row)))

(defun read-input ()
  (with-open-file (f "day18-input.txt")
    (when f (read-line f nil nil))))

(defun answer1 ()
  (count-safe-tiles (read-input) 40))

(defun answer2 ()
  (count-safe-tiles (read-input) 400000))
