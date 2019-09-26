

(defun read-blacklist (line)
  (let (*read-eval*)
    (with-input-from-string (s (substitute #\space #\- line))
      (loop for num = (read s nil nil)
         while num collect num))))

(defun read-input ()
  (with-open-file (f "day20-input.txt")
    (when f (loop for line = (read-line f nil nil)
               while line
               collect (read-blacklist line)))))

(defun compare-intervals (one other)
  (and (< (car one) (car other))
       (< (cadr one) (cadr other))))

(defun intersect (one other)
  (or (<= (car one) (- (car other) 1) (cadr one))
      (<= (car other) (- (car one) 1) (cadr other))))

(defun combine (one other)
  (list (car one) (cadr other)))

(defun merge-intervals (intervals)
  (loop with start = (sort intervals #'compare-intervals)
     with current = start
     while (cdr current)
     if (intersect (car current) (cadr current))
     do (setf (car current) (combine (car current) (cadr current))
              (cdr current) (cddr current))
     else do (setf current (cdr current))
     finally (return start)))

(defun find-first-open-ip (intervals)
  (let ((merged (merge-intervals intervals)))
    (if (< 0 (caar merged))
        0
        (+ 1 (cadar merged)))))
