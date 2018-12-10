(ql:quickload :cl-ppcre)
(load "../2018/utilities.lisp")

(defun read-candidates ()
  (read-lines "squares-with-three-sides-input.txt" 
              (lambda (line) 
                (multiple-value-bind (m rs) (cl-ppcre:scan-to-strings 
                                             "\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)\\s*" line)
                  (declare (ignore m))
                  (map 'list #'parse-integer rs)))))

(defun is-triangle (dimensions)
  (destructuring-bind (s1 s2 s3) dimensions
    (and (> (+ s1 s2) s3) (> (+ s1 s3) s2) (> (+ s2 s3) s1))))

(defun number-of-possible-triangles (candidates)
  (length (remove-if-not #'is-triangle candidates)))

(defun solution-part-1 () 
  (number-of-possible-triangles (read-candidates)))

(defun take (n l)
  (loop for i from 1 to n collect (car l) do (setf l (cdr l))))

(defun drop (n l)
  (loop for i from 1 to n do (setf l (cdr l)))
  l)

(defun transpose (mat)
  (if (null (car mat)) 
      nil
      (cons (mapcar #'car mat) (transpose (mapcar #'cdr mat)))))

(defun candidates-by-column (candidates)
  (let (triangles)
    (loop for remaining = candidates then (drop 3 remaining)
       while (not (null remaining))
       do (setf triangles (append (transpose (take 3 remaining)) triangles)))
    triangles))

(defun solution-part-2 ()
  (number-of-possible-triangles (candidates-by-column (read-candidates))))
