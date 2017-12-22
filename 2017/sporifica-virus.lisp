(defun turn-right (node)
  (let ((new-direction (case (car node)
                         (north 'east)
                         (east 'south)
                         (south 'west)
                         (west 'north))))
    (cons new-direction (cdr node))))

(defun turn-left (node)
  (let ((new-direction (case (car node)
                         (north 'west)
                         (west 'south)
                         (south 'east)
                         (east 'north))))
    (cons new-direction (cdr node))))

(defun turn-back (node)
  (let ((new-direction (case (car node)
                         (north 'south)
                         (west 'east)
                         (south 'north)
                         (east 'west))))
    (cons new-direction (cdr node))))

(defun adjust-direction (current grid)
  (if (gethash (cdr current) grid)
      (turn-right current)
      (turn-left current)))

(defun update-infection-status (current grid callback)
  (if (gethash (cdr current) grid)
      (remhash (cdr current) grid)
      (progn 
        (funcall callback)
        (setf (gethash (cdr current) grid) t))))

(defun get-infection-status (current grid)
  (let ((lookup (gethash (cdr current) grid)))
    (if lookup lookup 'C)))

(defun cycle-infection-status (current grid callback)
  (let ((status (get-infection-status current grid)))
    (let ((next-status (case status
                         (C 'W)
                         (W 'I)
                         (I 'F)
                         (F 'C))))
      (if (eq next-status 'I)
          (funcall callback))
      (setf (gethash (cdr current) grid) next-status))))

(defun adjust-direction-updated (current grid)
  (let ((status (get-infection-status current grid)))
;    (format t "status ~a~%" status)
    (case status
      (C (turn-left current))
      (W current)
      (I (turn-right current))
      (F (turn-back current)))))

(defun move-node (current)
  (let ((position (cdr current)))
    (let ((new-position (case (car current)
                          (north (cons (- (car position) 1) (cdr position)))
                          (south (cons (+ (car position) 1) (cdr position)))
                          (east (cons (car position) (+ (cdr position) 1)))
                          (west (cons (car position) (- (cdr position) 1))))))
      (cons (car current) new-position))))

(defun iter (current grid callback)
  (progn 
    (setf current (adjust-direction current grid))
    (update-infection-status current grid callback)
    (move-node current)))

(defun iter-updated (current grid callback)
  (progn 
;    (format t "~a ~a~%" current grid)
    (setf current (adjust-direction-updated current grid))
;    (format t "current updated ~a~%" current)
    (cycle-infection-status current grid callback)
    (move-node current)))

(defun collect-lines (filename)
  (let ((lines (with-open-file (f filename)
                 (when f 
                   (loop for line = (read-line f nil nil)
                      while line collect line)))))
    (map 'vector #'identity lines)))

(defun test-lines ()
  (collect-lines "sporifica-virus-test-input"))

(defun real-lines ()
  (collect-lines "sporifica-virus-input.txt"))

(defun center (dim)
  (floor dim 2))

(defun build-grid (lines)
  (let ((grid (make-hash-table :test 'equal))
        (rows (length lines))
        (cols (length (aref lines 0))))
    (loop for r from 0 to (- rows 1)
       do (loop for c from 0 to (- cols 1)
             when (char= (aref (aref lines r) c) #\#)
             do (setf (gethash (cons r c) grid) 'I)))
    (values (cons 'north (cons (center rows) (center cols))) grid)))

(defun part-1-solver (lines iterations)
  (multiple-value-bind (start grid) (build-grid lines)
    (let ((infection-events 0))
      (labels ((recur (current iterations)
                 (if (> iterations 0)
                     (let ((new-current 
                            (iter current grid (lambda () (incf infection-events)))))
                       (recur new-current (- iterations 1))))))
        (recur start iterations)
        infection-events))))

(defun part-2-solver (lines iterations)
  (multiple-value-bind (start grid) (build-grid lines)
    (let ((infection-events 0))
      (labels ((recur (current iterations)
                 (if (> iterations 0)
                     (let ((new-current
                            (iter-updated current grid (lambda () (incf infection-events)))))
                       (recur new-current (- iterations 1))))))
        (recur start iterations)
        infection-events))))

(defun test-part-1 ()
  (part-1-solver (test-lines) 70))

(defun solution-part-1 ()
  (part-1-solver (real-lines) 10000))

(defun test-part-2 ()
  (part-2-solver (test-lines) 10000000))

(defun solution-part-2 ()
  (part-2-solver (real-lines) 10000000))
