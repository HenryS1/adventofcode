(ql:quickload 'cl-ppcre)

(defun parse-input (line)
  (let* ((trimmed (subseq line 0 (- (length line) 1)))
         (syms (with-input-from-string (s trimmed)
                 (loop for sym = (read s nil nil)
                    while sym collect sym))))
    (values (cons (car syms) (car (last syms))) (list (caddr syms) (cadddr syms)))))

(defun add-relationship (one-other change relationships)
  (setf (gethash one-other relationships) change))

(defun add-guests (one-other guest-set)
  (progn (setf (gethash (car one-other) guest-set) t)
         (setf (gethash (cdr one-other) guest-set) t)))

(defun get-change-in-value (one other relationships)
  (let ((lookup (gethash (cons one other) relationships)))
    (case (car lookup)
      (gain (cadr lookup))
      (lose (- (cadr lookup))))))

(defun total-change-in-value (one other relationships)
  (+ (get-change-in-value one other relationships)
     (get-change-in-value other one relationships)))

(defun total-change (seating-plan relationships)
  (let ((total 0))
    (labels ((recur (fst rest)
               (if (null rest) 
                   (incf total
                         (total-change-in-value
                          (car seating-plan) (car (last seating-plan)) relationships))
                   (progn 
                     (incf total (total-change-in-value fst (car rest) relationships))
                     (recur (car rest) (cdr rest))))))
      (recur (car seating-plan) (cdr seating-plan))
      total)))

(defun fullp (table capacity) (= (hash-table-count table) capacity))

(defun find-optimal-seating-plan (guests relationships)
  (let (mx
        (total-guests (length guests))
        (seen (make-hash-table :test 'equal)))
    (labels ((recur (acc)
               (cond ((fullp seen total-guests)
                      (let ((change (total-change acc relationships)))
                        (if (or (not mx)
                                (> change mx))
                            (setf mx change))))
                     (t (loop for guest in guests 
                           when (not (gethash guest seen))
                           do (progn (setf (gethash guest seen) t)
                                     (recur (cons guest acc))
                                     (remhash guest seen)))))))
      (recur '()))
    mx))

(defun process-lines (filename callback)
  (with-open-file (f filename)
    (when f
      (loop for line = (read-line f nil nil)
         while line 
         do (funcall callback line)))))

(defun guest-list (guest-set)
  (loop for guest being the hash-keys of guest-set collect guest))

(defun build-guests-and-relationships (filename)
  (let ((guest-set (make-hash-table :test 'equal))
        (relationships (make-hash-table :test 'equal)))
    (process-lines filename (lambda (line)
                              (multiple-value-bind (one-other change) (parse-input line)
                                (add-relationship one-other change relationships)
                                (add-guests one-other guest-set))))
    (values (guest-list guest-set) relationships)))

(defun add-me (guests relationships)
  (loop for guest in guests
     do (progn 
          (add-relationship (cons 'me guest) (list 'gain 0) relationships)
          (add-relationship (cons guest 'me) (list 'gain 0) relationships)))
  (values (cons 'me guests) relationships))

(defun build-guests-and-relationships-with-me (filename)
  (multiple-value-bind (guests relationships) (build-guests-and-relationships filename)
    (add-me guests relationships)))

(defun part-1-solver (filename)
  (multiple-value-bind (guests relationships) (build-guests-and-relationships filename)
    (find-optimal-seating-plan guests relationships)))

(defun part-2-solver (filename)
  (multiple-value-bind (guests relationships) 
      (build-guests-and-relationships-with-me filename)
    (find-optimal-seating-plan guests relationships)))

(defun part-1-test ()
  (part-1-solver "guest-happiness-test-input"))

(defun part-2-test ()
  (part-2-solver "guest-happiness-test-input"))

(defun solution-part-1 ()
  (part-1-solver "guest-happiness-input.txt"))

(defun solution-part-2 ()
  (part-2-solver "guest-happiness-input.txt"))
