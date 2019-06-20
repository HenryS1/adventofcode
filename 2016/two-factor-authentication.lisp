(ql:quickload :cl-ppcre)

(defun parse-rect (line)
  (multiple-value-bind (match regs) (cl-ppcre:scan-to-strings "rect\\s+(\\d+)x(\\d+).*" line)
    (declare (ignore match))
    `(lambda (grid) (rect ,@(map 'list #'parse-integer (reverse regs)) grid))))

(defun parse-row (line)
  (multiple-value-bind (match regs) (cl-ppcre:scan-to-strings ".*row\\s+y=(\\d+)\\s+by\\s+(\\d+).*" line)
    (declare (ignore match))
    `(lambda (grid) (rotate-row ,@(map 'list #'parse-integer regs) grid))))

(defun parse-col (line)
  (multiple-value-bind (match regs) (cl-ppcre:scan-to-strings ".*column\\s+x=(\\d+)\\s+by\\s+(\\d+)" line)
    (declare (ignore match))
    `(lambda (grid) (rotate-col ,@(map 'list #'parse-integer regs) grid))))

(defun parse-command (line)
  (cond ((search "rect" line)
         (parse-rect line))
        ((search "row" line)
         (parse-row line))
        ((search "column" line)
         (parse-col line))))

(defun parse-commands ()
  (with-open-file (f "day8.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line collect (parse-command line)))))

(defun rect (rows cols grid)
  (loop for row from 0 to (1- rows)
     do (loop for col from 0 to (1- cols)
           do (setf (aref grid row col) #\#))
     finally (return grid)))

(defun rotate-row (row shift grid)
  (let ((temp (make-array (cadr (array-dimensions grid)))))
    (loop for col from 0 to (1- (cadr (array-dimensions grid)))
       do (setf (aref temp col) (aref grid row col)))
    (loop with cols = (cadr (array-dimensions grid))
      for col from 0 to (1- cols)
      for offset-index = (mod (- col shift) cols)
      do (setf (aref grid row col) (aref temp offset-index))
      finally (return grid))))

(defun transpose (grid)
  (loop with rows = (car (array-dimensions grid))
     with cols = (cadr (array-dimensions grid))
     with result = (make-array (list cols rows))
     for row from 0 to (1- rows)
     do (loop for col from 0 to (1- cols)
           do (setf (aref result col row) 
                    (aref grid row col)))
     finally (return result)))

(defun rotate-col (col shift grid)
  (transpose (rotate-row col shift (transpose grid))))

(defun lit-pixels (grid)
  (loop with rows = (car (array-dimensions grid))
     with cols = (cadr (array-dimensions grid))
     with total = 0
     for row from 0 to (1- rows)
     do (loop for col from 0 to (1- cols)
           when (char= (aref grid row col) #\#)
           do (incf total))
     finally (return total)))

(defun part-1-test ()
  (let ((grid (make-array '(3 7) :initial-element 0))
        (commands (list 
                   (lambda (grid) (rect 2 3 grid))
                   (lambda (grid) (rotate-col 1 2 grid))
                   ;; (lambda (grid) (rotate-row 0 4 grid))
                   ;; (lambda (grid) (rotate-col 1 1 grid))
                   )))
    (loop for command in commands do (setf grid (funcall command grid))
       finally (format t "~a~%" grid))))

(defun take (n l)
  (loop for rest = l then (cdr rest)
     for i from 1 to n
     collect (car rest)))

(defun part-1 ()
  (let ((grid (make-array '(6 50) :initial-element #\.))
        (commands (parse-commands)))
    (loop for command in commands do (funcall (eval command) grid)
         (format t "COMMAND ~a~%" command)
         (format t "GRID~% ~a~%" grid)
       finally (return (lit-pixels grid)))))
