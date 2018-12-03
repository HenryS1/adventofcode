(defun parse-instructions (lines)
  (mapcar (lambda (line) (parse-integer line :junk-allowed t)) lines))

(defun collect-lines ()
  (with-open-file (f "chronal-calibration-input.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line
         collect line))))

(defun adjust-frequency (start instructions)
  (+ start (reduce #'+ instructions)))

(defun solution-part-1 ()
  (adjust-frequency 0 (parse-instructions (collect-lines))))

(defun find-repeated-frequency (current instructions)
  (let ((seen (make-hash-table)))
    (loop while t 
       do (loop for instruction in instructions
             do (setf (gethash current seen) t)
               (incf current instruction)
             when (gethash current seen)
             do (return-from find-repeated-frequency current)))))

(defun solution-part-2 ()
  (find-repeated-frequency 0 (parse-instructions (collect-lines))))
