(defpackage :day14
  (:use 
   :cl 
   :iterate 
   :ironclad
   :anaphora 
   :flexi-streams
   :pears
   :metabang-bind
   :queue)
  (:export
   :parse-rocks
   :roll-rocks-north
   :calculate-north-side-load
   :cycle-rocks))

(in-package :day14)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-rocks ()
  (fmap #l(coerce %rocks 'vector)
        (sep-by (many1 #l(not (newlinep %c)))
                (many1 #'newlinep))))

(defun read-rocks-from-file (filename)
  (parse-file filename (parse-rocks)))

(defun roll-rocks-north (rocks)
  (loop with available-row = (make-array (length (aref rocks 0)) 
                                         :initial-element 0 :element-type 'fixnum)
        for row-num from 0
        for row across rocks
        do (loop for c across row
                 for col from 0
                 when (char= c #\#) 
                   do (setf (aref available-row col) (+ row-num 1))
                 when (char= c #\O)
                   do (rotatef (aref (aref rocks (aref available-row col)) col)
                               (aref (aref rocks row-num) col))
                      (incf (aref available-row col)))))

(defun roll-rocks-south (rocks)
  (loop with rows = (length rocks)
        with available-row = (make-array (length (aref rocks 0)) 
                                         :initial-element (- rows 1) :element-type 'fixnum)
        for row-num from (- rows 1) downto 0
        for row = (aref rocks row-num)
        do (loop for c across row
                 for col from 0
                 when (char= c #\#) 
                   do (setf (aref available-row col) (- row-num 1))
                 when (char= c #\O)
                   do (rotatef (aref (aref rocks (aref available-row col)) col)
                               (aref (aref rocks row-num) col))
                      (decf (aref available-row col)))))

(defun roll-rocks-west (rocks)
  (loop with rows = (length rocks)
        with cols = (length (aref rocks 0))
        with available-col = (make-array (length rocks) :initial-element 0 :element-type 'fixnum)
        for col-num from 0 to (- cols 1)
        do (loop for row from 0 to (- rows 1)
                 for c = (aref (aref rocks row) col-num)
                 when (char= c #\#)
                   do (setf (aref available-col row) (+ col-num 1))
                 when (char= c #\O)
                   do (rotatef (aref (aref rocks row) (aref available-col row))
                               (aref (aref rocks row) col-num))
                      (incf (aref available-col row)))))

(defun roll-rocks-east (rocks)
  (loop with rows = (length rocks)
        with cols = (length (aref rocks 0))
        with available-col = (make-array (length rocks) 
                                         :initial-element (- cols 1)
                                         :element-type 'fixnum)
        for col-num from (- cols 1) downto 0
        do (loop for row from 0 to (- rows 1)
                 for c = (aref (aref rocks row) col-num)
                 when (char= c #\#)
                   do (setf (aref available-col row) (- col-num 1))
                 when (char= c #\O)
                   do (rotatef (aref (aref rocks row) (aref available-col row))
                               (aref (aref rocks row) col-num))
                      (decf (aref available-col row)))))

(defun cycle-rocks (rocks)
  (roll-rocks-north rocks)
  (roll-rocks-west rocks)
  (roll-rocks-south rocks)
  (roll-rocks-east rocks))

(defun hash-rocks (rocks)
  (let ((digest (make-digest :md5)))
    (loop for row across rocks
          do (update-digest digest (string-to-octets row))
          finally (return (byte-array-to-hex-string (produce-digest digest))))))

(defun to-cache-key (rocks)
  (hash-rocks rocks))

(defun print-rocks (rocks)
  (loop for row across rocks
        do (format t "~a~%" row)))

(defun find-init-and-repeat (rocks)
  (let ((cache (make-hash-table :test 'equal)))
    (loop for i from 0 to 1000000
          for key = (hash-rocks rocks)
          until (gethash key cache)
          do (setf (gethash key cache) i)
             (cycle-rocks rocks)
          finally (return (cons (gethash key cache) (- i (gethash key cache)))))))

(defun calculate-north-side-load (rocks)
  (loop with rows = (length rocks)
        with total-load = 0
        for row-num from 0
        for row across rocks
        do (loop for c across row
                 when (char= c #\O)
                   do (incf total-load (- rows row-num)))
        finally (return total-load)))

(defun part1 ()
  (let ((rocks (read-rocks-from-file "input14")))
    (roll-rocks-north rocks)
    (calculate-north-side-load rocks)))

(defun part2 ()
  (bind ((rocks (read-rocks-from-file "input14"))
         ((init . repeat) (find-init-and-repeat rocks))
         (remainder (mod (- 1000000000 init) repeat)))
    (loop for i from 1 to remainder do (cycle-rocks rocks)
          finally (return (calculate-north-side-load rocks)))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
