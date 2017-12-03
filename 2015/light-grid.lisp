(ql:quickload 'cl-ppcre)

(defun starts-with (one other)
  (let ((len-one (length one))
        (len-other (length other)))
    (if (< len-one len-other)
        nil
        (loop for i from 0 to (- len-other 1)
           when (char/= (aref one i) (aref other i))
           do (return-from starts-with nil)))
    t))

(defun toggle-type (line)
  (cond ((starts-with line "turn on") 'turn-on)
        ((starts-with line "turn off") 'turn-off)
        ((starts-with line "toggle") 'toggle)
        (t (error "unexpected start of line"))))

(defun parse-input (line)
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings "[a-zA-Z\\s]*(\\d+),(\\d+)[a-zA-Z\\s]*(\\d+),(\\d+)[a-zA-Z\\s]*"
                                line)
    (declare (ignorable match))
    (let ((coords (map 'list #'parse-integer registers))
          (action (toggle-type line)))
      (values action coords))))

(defun make-grid ()
  (make-array (list 1000 1000) :element-type 'integer))

(defun process-coords (coords callback)
    (destructuring-bind (x1 y1 x2 y2) coords
      (loop for x from x1 to x2
         do (loop for y from y1 to y2
               do (funcall callback x y)))))

(defun turn-on-lights (grid coords)
  (process-coords coords 
                  (lambda (x y)
                    (setf (aref grid x y) 1))))

(defun turn-off-lights (grid coords)
  (process-coords coords
                  (lambda (x y)
                    (setf (aref grid x y) 0))))

(defun toggle (grid coords)
  (process-coords coords
                  (lambda (x y)
                    (let ((current (aref grid x y)))
                      (if (= current 0)
                          (setf (aref grid x y) 1)
                          (setf (aref grid x y) 0))))))

(defun process-line (grid line)
  (multiple-value-bind (action coords) (parse-input line)
    (case action
      (turn-on (turn-on-lights grid coords))
      (turn-off (turn-off-lights grid coords))
      (toggle (toggle grid coords)))))

(defun add-up-brightness (grid)
  (let ((brightness 0))
    (loop for x from 0 to 999
       do (loop for y from 0 to 999
             do (incf brightness (aref grid x y))))
    brightness))

(defun increase-brightness (grid coords increase)
  (process-coords coords 
                  (lambda (x y)
                    (incf (aref grid x y) increase))))

(defun decrease-brightness (grid coords)
  (process-coords coords
                  (lambda (x y)
                    (if (> (aref grid x y) 0)
                        (decf (aref grid x y))))))

(defun process-line-improved (grid line)
  (multiple-value-bind (action coords) (parse-input line)
    (case action
      (turn-on (increase-brightness grid coords 1))
      (turn-off (decrease-brightness grid coords))
      (toggle (increase-brightness grid coords 2)))))

(defun test-line (line callback)
  (let ((grid (make-grid)))
    (funcall callback grid line)
    (add-up-brightness grid)))

(defun test-line-1 (line)
  (test-line line #'process-line))

(defun test-line-2 (line)
  (test-line line #'process-line-improved))

(defun solve (callback)
  (let ((grid (make-grid)))
    (with-open-file (f "light-grid-input.txt")
      (when f
        (loop for line = (read-line f nil nil)
           while line 
           do (funcall callback grid line))))
    (add-up-brightness grid)))

(defun solution-part-1 () (solve #'process-line))

(defun solution-part-2 () (solve #'process-line-improved))
