(ql:quickload 'cl-ppcre)

(defun extract-first-match (pattern line parser)
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings pattern line)
    (declare (ignore match))
    (funcall parser (aref registers 0))))

(defun extract-first-sym (pattern line)
  (extract-first-match pattern line (lambda (s) (intern (string-upcase s)))))

(defun extract-first-num (pattern line)
  (extract-first-match pattern line #'parse-integer))

(defun get-state-name (line)
  (extract-first-sym "In state (\\w):" line))

(defun parse-tag (line)
  (extract-first-num "If the current value is (\\d+):" line))

(defun parse-write-value (line)
  (extract-first-num "Write the value (\\d+)." line))

(defun parse-direction (line)
  (extract-first-sym "Move one slot to the (\\w+)." line))

(defun parse-next (line)
  (extract-first-sym "Continue with state (\\w)." line))

(defun parse-initial-state (line)
  (extract-first-sym "Begin in state (\\w+)." line))

(defun parse-checksum-steps (line)
  (extract-first-num "Perform a diagnostic checksum after (\\d+) steps." line))

(defun parse-actions (strm)
  (let ((tag (parse-tag (read-line strm nil nil)))
        (write-value (parse-write-value (read-line strm nil nil)))
        (direction (parse-direction (read-line strm nil nil)))
        (next-state (parse-next (read-line strm nil nil))))
    (declare (ignorable tag))
    (list write-value direction next-state)))

(defun parse-state (strm)
  (let ((name (get-state-name (read-line strm nil nil)))
        (first-actions (parse-actions strm))
        (second-actions (parse-actions strm)))
    (read-line strm nil nil)
    (cons name (list first-actions second-actions))))

(defun parse-states (strm)
  (loop while (peek-char t strm nil nil)
     collect (parse-state strm)))

(defun parse-input (filename)
  (let ((states (make-hash-table)))
    (with-open-file (f filename)
      (when f
        (let ((initial-state (parse-initial-state (read-line f nil nil)))
              (checksum-steps (parse-checksum-steps (read-line f nil nil))))
          (read-line f nil nil)
          (loop for state in (parse-states f)
             do (setf (gethash (car state) states) state))
          (list initial-state checksum-steps states))))))

(defun get-real-states ()
  (parse-input "halting-problem-input.txt"))

(defun get-value (position tape)
  (or (gethash position tape) 0))

(defun move (position direction)
  (case direction
    (left (- position 1))
    (right (+ position 1))))

(defun zero-action (position state tape states)
  (take-action position state tape states #'cadr))

(defun one-action (position state tape states)
  (take-action position state tape states #'caddr))

(defun take-action (position state tape states accessor)
  (destructuring-bind (write-value direction next-state) (funcall accessor state)
    (setf (gethash position tape) write-value)
    (values (move position direction) (gethash next-state states))))

(defun step-state (position state tape states)
  (let ((val (get-value position tape)))
    (if (= val 0)
        (zero-action position state tape states)
        (one-action position state tape states))))

(defun count-ones (tape)
  (let ((total 0))
    (loop for v being the hash-values of tape 
       do (incf total v))
    total))

(defun iterate (position state tape states iterations)
  (loop for i from 1 to iterations 
     do (multiple-value-bind (next-position next-state) 
            (step-state position state tape states)
          (setf position next-position)
          (setf state next-state)))
  (count-ones tape))

(defun part-1-solver (filename)
  (destructuring-bind (initial-state checksum-steps states) 
      (parse-input filename)
    (iterate 0 (gethash initial-state states) (make-hash-table) states checksum-steps)))

(defun test-part-1 ()
  (part-1-solver "halting-problem-test-input"))

(defun solution-part-1 ()
  (part-1-solver "halting-problem-input.txt"))
