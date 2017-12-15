(defun get-input (filename)
  (with-open-file (f filename)
    (when f
      (coerce (read-line f) 'list))))

(defun handle-garbage (input garbage-callback)
  (if (null input)
      nil
      (case (car input)
        (#\> (cdr input))
        (#\! (handle-garbage (cddr input) garbage-callback))
        (otherwise (progn 
                     (funcall garbage-callback)
                     (handle-garbage (cdr input) garbage-callback))))))

(defun score-group (input nesting-level garbage-callback)
  (if (null input)
      (values 0 nil)
      (case (car input)
        (#\< (score-group (handle-garbage (cdr input) garbage-callback) 
                          nesting-level garbage-callback))
        (#\{ (multiple-value-bind (score rest) 
                 (score-group (cdr input) (+ nesting-level 1) garbage-callback)
               (multiple-value-bind (next-score next-rest) 
                   (score-group rest nesting-level garbage-callback)
                 (values (+ score next-score) next-rest))))
        (#\} (values nesting-level (cdr input)))
        (otherwise (score-group (cdr input) nesting-level garbage-callback)))))

(defun no-op () ())

(defun test-part-1 ()
  (and (= (score-group (coerce "{{<ab>},{<ab>},{<ab>},{<ab>}}" 'list) 0 #'no-op) 9)
       (= (score-group (coerce "{{{},{},{{}}}}" 'list) 0 #'no-op) 16)))

(defun solution-part-1 ()
  (score-group (get-input "stream-processing-input.txt") 0 #'no-op))

(defun part-2-solver (input)
  (let ((count 0))
    (score-group input 0 (lambda () (incf count)))
    count))

(defun test-part-2 (str)
  (part-2-solver (coerce str 'list)))

(defun solution-part-2 ()
  (part-2-solver (get-input "stream-processing-input.txt")))

