(ql:quickload :cl-ppcre)

(defun read-date (line)
  (multiple-value-bind (m rs) (cl-ppcre:scan-to-strings 
                               "\\[\\d{4}-\\d{2}-\\d{2} (\\d{2}):(\\d{2})\\].*"
                               line)
    (declare (ignore m))
    (map 'list #'parse-integer rs)))

(defun read-guard-id (line)
  (multiple-value-bind (m r) (cl-ppcre:scan-to-strings
                               "\\[.*\\] Guard #(\\d+).*" line)
    (declare (ignore m))
    (car (map 'list #'parse-integer r))))

(defun read-event (line)
  (cond ((search "shift" line)
         (list 'begin-shift (read-date line) (read-guard-id line)))
        ((search "wakes" line)
         (list 'wake (read-date line)))
        ((search "falls" line)
         (list 'sleep (read-date line)))
        (t nil)))

(defun get-minutes (time) (cadr time))

(defun get-hours (time) (car time))

(defun get-next-start-time (time)
  (if (> (get-hours time) 0)
      0
      (get-minutes time)))

(defun get-time (event)
  (cadr event))

(defun get-guard-id (guard)
  (caddr guard))

(defmacro total-time-asleep (guard)
  `(car ,guard))

(defun update-asleep-time (guard start-time end-time)
  (let ((time-counts (cadr guard)))
    (loop for minute from start-time to (1- end-time)
       do (incf (total-time-asleep guard))
         (when (not (gethash minute time-counts))
           (setf (gethash minute time-counts) 0))
         (incf (gethash minute time-counts)))))

(defun process-events (events guards)
  (let (current-guard start-time)
    (loop for event in events
       do (case (car event)
            (begin-shift 
             (let ((guard-id (caddr event)))
               (if (not (gethash guard-id guards))
                   (setf (gethash guard-id guards) (list 0 (make-hash-table) guard-id)))
               (setf current-guard (gethash guard-id guards))))
            (wake (let ((end-time (get-minutes (get-time event))))
                     (update-asleep-time current-guard start-time end-time)))
            (sleep (setf start-time (get-next-start-time (get-time event))))))))

(defun find-sleepiest-guard (guards)
  (let ((most-time 0)
        best-guard)
    (loop for guard being the hash-values of guards
       when (> (total-time-asleep guard) most-time)
       do (setf most-time (total-time-asleep guard)) 
         (setf best-guard guard))
    best-guard))

(defun find-most-slept-minute (guard)
  (let ((time-counts (cadr guard))
        (best-count 0)
        (best-minute -1))
    (loop for minute being the hash-keys of time-counts using (hash-value count)
       when (> count best-count)
       do (setf best-count count)
         (setf best-minute minute))
    (values best-minute best-count)))

(defun find-sleepiest-guard-for-a-minute (guards)
  (let ((most-sleep 0)
        (best-minute -1)
        best-guard)
    (loop for guard being the hash-values of guards 
       do (multiple-value-bind (minute count) (find-most-slept-minute guard)
            (when (> count most-sleep)
              (setf best-minute minute)
              (setf most-sleep count)
              (setf best-guard guard))))
    (cons best-minute (get-guard-id best-guard))))

(defun read-lines (filename)
  (with-open-file (f filename)
    (when f
      (loop for line = (read-line f nil nil)
         while line 
         collect line))))

(defun solution-part-1 ()
  (let ((events (mapcar #'read-event (sort (read-lines "repose-record-input.txt") #'string<)))
        (guards (make-hash-table :test 'equal)))
    (process-events events guards)
    (let ((sleepiest-guard (find-sleepiest-guard guards)))
      (cons (get-guard-id sleepiest-guard) (find-most-slept-minute sleepiest-guard)))))

(defun solution-part-2 ()
  (let ((events (mapcar #'read-event (sort (read-lines "repose-record-input.txt") #'string<)))
        (guards (make-hash-table :test 'equal)))
    (process-events events guards)
    (find-sleepiest-guard-for-a-minute guards)))
