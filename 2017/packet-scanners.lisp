(ql:quickload 'cl-ppcre)

(defun read-scanner (line)
  (multiple-value-bind (match registers) (cl-ppcre:scan-to-strings "(\\d+)[^\\d]+(\\d+)" line)
    (declare (ignorable match))
    (cons 'down (cons 0 (map 'list #'parse-integer registers)))))

(defun move (direction position range)
  (case direction 
    (up (let ((new-position (- position 1)))
          (if (= new-position 0)
              (values 'down new-position)
              (values 'up new-position))))
    (down (let ((new-position (+ position 1)))
            (if (= new-position (- range 1))
                (values 'up new-position)
                (values 'down new-position))))))

(defun update-scanner (scanner)
  (destructuring-bind (direction position depth range) scanner
    (declare (ignorable depth))
    (multiple-value-bind (new-direction new-position) (move direction position range)
      (cons new-direction (cons new-position (cddr scanner))))))

(defun update-scanners (scanners)
  (mapcar #'update-scanner scanners))

(defun severity-increment (scanners)
  (destructuring-bind (direction position depth range) (car scanners)
    (declare (ignorable direction))
    (if (= position 0)
        (* depth range)
        0)))

(defun find-total-severity (scanners)
  (labels ((recur (current-depth scanners total)
             (if (null scanners)
                 total
                 (destructuring-bind (direction position depth range) (car scanners)
                   (declare (ignorable direction position range))
                   (let ((additional-severity
                          (if (= current-depth depth) (severity-increment scanners) 0))
                         (new-scanners (if (= current-depth depth)
                                           (update-scanners (cdr scanners))
                                           (update-scanners scanners))))
                     (recur (+ current-depth 1) new-scanners (+ total additional-severity)))))))
    (recur 0 scanners 0)))

(defun max-depth (scanners)
  (reduce (lambda (mx sc) (if (> (cadddr sc) mx) (caddr sc) mx)) scanners))

(defun is-caught (index scanners)
  (loop for scanner in scanners
     while (<= (caddr scanner) index)
     do (if (and (= (caddr scanner) index)
                 (= (cadr scanner) 0))
            (return-from is-caught t)))
  nil)

(defun update-indices (start-times)
  (mapcar (lambda (start-time) (cons (+ (car start-time) 1) (cdr start-time))) start-times))

(defun find-solution (filtered max-depth)
  (cond ((null filtered) nil)
        ((> (caar filtered) max-depth)
         (car filtered))
        (t (has-solution (cdr filtered) max-depth))))

(defun dont-get-caught (scanners max-depth)
  (labels ((recur (start-times scanners max-start-time)
             (let ((filtered (remove-if 
                              (lambda (index) (is-caught (car index) scanners)) start-times)))
               (let ((solution (find-solution filtered max-depth)))
                 (if solution
                     solution
                     (recur (cons (cons 0 (+ max-start-time 1))
                                  (update-indices filtered)) 
                            (update-scanners scanners) 
                            (+ max-start-time 1)))))))
    (recur (list '(0 . 0)) scanners 0)))

(defun collect-scanners (filename)
  (with-open-file (f filename)
    (when f
      (loop for line = (read-line f nil nil)
         while line
         collect (read-scanner line)))))

(defun find-max-depth (scanners)
  (reduce (lambda (mx sc) (if (> (caddr sc) mx) (caddr sc) mx)) scanners :initial-value 0))

(defun part-1-solver (filename)
  (find-total-severity (collect-scanners filename)))

(defun part-2-solver (filename)
  (let ((scanners (collect-scanners filename)))
    (dont-get-caught scanners (find-max-depth scanners))))

(defun test-part-1 ()
  (part-1-solver "packet-scanner-test-input"))

(defun test-part-2 ()
  (part-2-solver "packet-scanner-test-input"))

(defun solution-part-1 ()
  (part-1-solver "packet-scanner-input.txt"))

(defun solution-part-2 ()
  (part-2-solver "packet-scanner-input.txt"))
