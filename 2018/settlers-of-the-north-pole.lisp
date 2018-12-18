(load "utilities.lisp")

(defun count-adjacent (acre map square-type)
  (loop with rows = (length map)
     with cols = (length (aref map 0))
     for row from (1- (car acre)) to (1+ (car acre))
     sum (loop for col from (1- (cdr acre)) to (1+ (cdr acre))
            count (and (<= 0 row (1- rows))
                       (<= 0 col (1- cols))
                       (not (and (= row (car acre))
                                 (= col (cdr acre))))
                       (char= (aref (aref map row) col) square-type)))))

(defun fills-with-trees (acre map)
  (and (char= (aref (aref map (car acre)) (cdr acre)) #\.)
             (>= (count-adjacent acre map #\|) 3)))

(defun becomes-lumberyard (acre map)
  (and (char= (aref (aref map (car acre)) (cdr acre)) #\|)
             (>= (count-adjacent acre map #\#) 3)))

(defun becomes-open (acre map)
  (and (char= (aref (aref map (car acre)) (cdr acre)) #\#)
             (or (< (count-adjacent acre map #\|) 1)
                 (< (count-adjacent acre map #\#) 1))))

(defun next-acre (acre map)
  (cond ((fills-with-trees acre map) #\|)
        ((becomes-lumberyard acre map) #\#)
        ((becomes-open acre map) #\.)
        (t (aref (aref map (car acre)) (cdr acre)))))

(defun print-map (map)
  (loop for row across map
     do (format t "~a~%" (coerce row 'string))))

(defun next-state (map)
  (loop for acres across map
     with next-map = (make-array (length map))
     for row = 0 then (1+ row)
     for next-acres = (make-array (length acres))
     do (loop for col from 0 to (1- (length acres))
           do (setf (aref next-acres col) (next-acre (cons row col) map)))
       (setf (aref next-map row) next-acres)
     finally (return next-map)))

(defparameter *input-file* "settlers-of-the-north-pole-input.txt")
(defparameter *test-input-file* "settlers-of-the-north-pole-test-input.txt")

(defun read-input (filename)
  (coerce (read-lines filename (lambda (line) (map 'vector #'identity line))) 'vector))

(defun count-squares (map square-type)
  (loop for row across map
     sum (count-if (lambda (square) (char= square square-type)) row)))

(defun resource-area-value (map)
  (let ((trees (count-squares map #\|))
        (lumberyards (count-squares map #\#)))
    (format t "trees ~a lumberyards ~a~%" trees lumberyards)
    (* trees lumberyards)))

(defun test-1 ()
  (loop for map = (read-input *test-input-file*) then (next-state map)
     for round from 1 to 10
     do (print-map map)
       (format t "~%")
     finally (print-map map)
       (return (resource-area-value map))))

(defun solution-part-1 ()
  (loop for map = (read-input *input-file*) then (next-state map)
     for round from 1 to 10
     finally (return (resource-area-value map))))

(defun solution-part-2 ()
  (loop for map = (read-input *input-file*) then (next-state map)
     for round from 1 to 1280
     finally (return (resource-area-value map))))
