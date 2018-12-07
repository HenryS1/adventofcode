(load "utilities.lisp")

(defun read-instructions ()
  (read-lines "sum-of-its-parts-input.txt" 
              (lambda (line) (subseq (remove-if-not #'upper-case-p line) 1))))

(defun collect-tasks (instructions)
  (map 'string #'identity 
       (hash-keys (make-set (reduce (lambda (a b) (concatenate 'string a b)) instructions)))))

(defun find-available (tasks instructions seen)
  (let ((available (make-set tasks)))
    (loop for task across tasks 
       when (gethash task seen)
       do (remhash task available))
    (loop for instruction in instructions
       when (not (gethash (aref instruction 0) seen))
       do (remhash (aref instruction 1) available))
    (sort (hash-keys available) #'char<)))

(defun find-ordering (instructions)
  (let ((tasks (collect-tasks instructions))
        ordering
        (seen (make-hash-table)))
    (loop while (< (length ordering) 
                   (length tasks))
       do (let ((available (find-available tasks instructions seen)))
            (setf (gethash (car available) seen) t)
            (push (car available) ordering)))
    (map 'string #'identity (reverse ordering))))

(defun solution-part-1 ()
  (find-ordering (read-instructions)))

(defun time-for-task (task)
  (+ (- (char-code task) 64) 60))

(defun find-available-with-time (tasks instructions started done)
  (let ((available (make-set tasks)))
    (loop for task across tasks 
       when (gethash task started)
       do (remhash task available))
    (loop for instruction in instructions
       when (not (gethash (aref instruction 0) done))
       do (remhash (aref instruction 1) available))
    (sort (hash-keys available) #'char<)))

(defun find-ordering-with-time-and-help (instructions num-workers)
  (let ((tasks (collect-tasks instructions))
        (started (make-hash-table))
        (workers (loop for i from 1 to num-workers collect (list 0 #\$)))
        (total-time -1)
        (done (make-hash-table)))
    (loop while (< (hash-table-count done)
                   (1+ (length tasks)))
       do (setf workers (mapcar (lambda (el) (list (max 0 (1- (car el))) (cadr el))) workers))
         (incf total-time)
         (loop for b in workers
            when (= (car b) 0)
            do (setf (gethash (cadr b) done) t))
       when (some (lambda (b) (= (car b) 0)) workers)
       do (let ((available (find-available-with-time tasks instructions started done)))
            (loop for ws = workers then (cdr ws)
               while ws
               when (and available (= (caar ws) 0))
               do (setf (car ws) (list (time-for-task (car available)) (car available)))
                 (setf (gethash (car available) started) t)
                 (pop available))))
    total-time))

(defun solution-part-2 ()
  (find-ordering-with-time-and-help (read-instructions) 5))
