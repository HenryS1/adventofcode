(ql:quickload 'cl-ppcre)

(defun parse-input (line)
  (let (*read-eval*) 
    (with-input-from-string (s line)
      (loop for num = (read s nil nil)
         while num collect num))))

(defun checksum (nums)
  (let ((mx 0)
        (mn 10000000000000000000))
    (loop for num in nums
       do (progn (if (>= num mx)
                     (setf mx num))
                 (if (<= num mn)
                     (setf mn num))))
    (- mx mn)))

(defun process-rows (callback)
  (with-open-file (f "checksum-input.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line 
         do (funcall callback (parse-input line))))))

(defun mx-mn-checksum ()
  (let ((total 0))
    (process-rows (lambda (nums)
                    (let ((row-checksum (checksum nums)))
                      (incf total row-checksum))))
    total))

(defun evenly-divisible-numbers (nums)
  (loop for num in nums 
     when (not (null (cdr nums)))
     do (loop for other in (cdr nums)
           when (or (and (< other num) (= (mod num other) 0))
                    (and (< num other) (= (mod other num) 0)))
           do (return-from evenly-divisible-numbers (cons (max num other)
                                                          (min num other)))))
  '(0 . 0))

(defun solution-part-1 ()
  (mx-mn-checksum))

(defun evenly-divisble-sum ()
  (let ((total 0))
    (process-rows (lambda (nums)
                    (let ((evenly-divisble (evenly-divisible-numbers nums)))
                      (incf total (/ (car evenly-divisble)
                                     (cdr evenly-divisble))))))
    total))

(defun solution-part-2 ()
  (evenly-divisble-sum))
