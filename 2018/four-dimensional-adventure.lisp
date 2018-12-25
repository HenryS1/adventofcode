(load "utilities.lisp")
(load "union-find.lisp")
(ql:quickload :cl-ppcre)

(defun read-coordinate (line)
  (map 'vector #'parse-integer (cl-ppcre:split "," line)))

(defparameter *input-file* "four-dimensional-adventure-input.txt")
(defparameter *test-input-file* "four-dimensional-adventure-test-input.txt")

(defun read-input (filename)
  (coerce (read-lines filename #'read-coordinate) 'vector))

(defun manhattan-distance (one other)
  (loop for i across one 
     for j across other
     sum (abs (- i j))))

(defun are-connected (one other)
  (<= (manhattan-distance one other) 3))

(defun find-constellations (coordinates)
  (loop with uf = (make-union-find (length coordinates))
     for one across coordinates
     for i = 0 then (1+ i)
     do (loop for j from (1+ i) to (1- (length coordinates))
           for other = (aref coordinates j)
           when (are-connected one other)
           do (uf-union uf i j))
     finally (return (components uf))))

