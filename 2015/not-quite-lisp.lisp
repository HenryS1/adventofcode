(defun next-floor (current-floor next-char)
  (case next-char
    (#\( (+ current-floor 1))
    (#\) (- current-floor 1))))

(defun find-floor (current-floor instructions callback)
  (if (null instructions)
      current-floor
      (let ((next (next-floor current-floor (car instructions))))
        (funcall callback next)
        (find-floor next (cdr instructions) callback))))

(defun get-input (file)
  (with-open-file (f file)
    (when f
      (read-line f))))

(defun find-basement (instructions)
  (let ((position 1))
    (find-floor 0 instructions 
              (lambda (fl) 
                (if (= fl -1)
                    (return-from find-basement position)
                    (incf position))))))

(defun solution-part-1 ()
  (let ((instructions (coerce (get-input "not-quite-lisp-input.txt") 'list)))
    (find-floor 0 instructions (lambda (fl) (declare (ignorable fl))))))

(defun solution-part-2 ()
  (let ((instructions (coerce (get-input "not-quite-lisp-input.txt") 'list)))
    (find-basement instructions)))
