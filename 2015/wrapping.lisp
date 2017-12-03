(ql:quickload 'cl-ppcre)

(defun parse-input (line)
  (mapcar #'parse-integer (cl-ppcre:split "x" line)))

(defun required-paper (dimensions)
  (let ((sides (list 
                (* (car dimensions) (cadr dimensions))
                (* (car dimensions) (caddr dimensions))
                (* (cadr dimensions) (caddr dimensions)))))
    (+ (reduce #'min sides) (* 2 (reduce #'+ sides)))))

(defun volume (dimensions)
  (reduce #'* dimensions))

(defun smallest-perimeter (dimensions)
  (let ((perimeters (list 
                     (+ (* 2 (car dimensions)) (* 2 (cadr dimensions)))
                     (+ (* 2 (car dimensions)) (* 2 (caddr dimensions)))
                     (+ (* 2 (cadr dimensions)) (* 2 (caddr dimensions))))))
    (reduce #'min perimeters)))

(defun required-ribbon (dimensions)
  (+ (volume dimensions) 
     (smallest-perimeter dimensions)))

(defun process-all-input (all-dimensions callback)
  (if (null all-dimensions)
      nil
      (progn
        (funcall callback (car all-dimensions))
        (process-all-input (cdr all-dimensions) callback))))

(defun total-paper (all-dimensions)
  (let ((total 0))
    (process-all-input all-dimensions 
                       (lambda (dimensions) 
                         (incf total (required-paper dimensions))))
    total))

(defun total-ribbon (all-dimensions)
  (let ((total 0))
    (process-all-input all-dimensions
                       (lambda (dimensions)
                         (incf total (required-ribbon dimensions))))
    total))

(defun get-all-dimensions (file)
  (with-open-file (f file)
    (when f
      (loop for line = (read-line f nil nil)
         while line collect (parse-input line)))))

(defun solve (callback)
  (let ((all-dimensions (get-all-dimensions "wrapping-input.txt")))
    (funcall callback all-dimensions)))

(defun solution-part-1 () (solve #'total-paper))

(defun solution-part-2 () (solve #'total-ribbon))
