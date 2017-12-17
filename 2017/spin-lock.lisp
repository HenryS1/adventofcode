(setf *print-circle* t)

(defun step-forward (buffer steps)
  (if (= steps 0)
      buffer
      (step-forward (cdr buffer) (- steps 1))))

(defun insert-in-buffer (buffer element)
  (progn (setf (cdr buffer) (cons element (cdr buffer)))
         (cdr buffer)))

(defun insert-and-spin (steps stop-value)
  (let ((buffer (list 0)))
    (setf (cdr buffer) buffer)
    (labels ((insert-elements (i)
               (if (= i stop-value)
                   buffer
                   (let ((stepped (step-forward buffer steps)))
                     (setf buffer (insert-in-buffer stepped i))
                     (insert-elements (+ i 1))))))
      (insert-elements 1))))

(defun insert-position (position len steps)
  (mod (+ position steps) len))

(defun step-insert (steps stop-value)
  (labels ((recur (i position value-after-zero)
             (if (= i stop-value)
                 value-after-zero
                 (let* ((new-position (insert-position position i steps))
                        (new-after-zero (if (= new-position 0) i value-after-zero)))
                   (recur (+ i 1) (+ new-position 1) new-after-zero)))))
    (recur 1 0 0)))

(defun test-value-for-part-2 ()
  (insert-and-spin 3 2018))

(defun test-part-2 ()
  (step-insert 3 2018))

(defun part-1-solver (steps)
  (cadr (insert-and-spin steps 2018)))

(defun test-part-1 ()
  (part-1-solver 3))

(defun solution-part-1 ()
  (part-1-solver 314))

(defun solution-part-2 ()
  (step-insert 314 50000001))
