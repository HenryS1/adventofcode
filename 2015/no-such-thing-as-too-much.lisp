(defun remove-first (el containers)
  (if (null containers)
      nil
      (if (= el (car containers))
          (cdr containers)
          (cons (car containers) (remove-first el (cdr containers))))))

(defun ways-to-store-it (amount containers)
  (let ((count 0))
    (labels ((recur (amount containers)
                   (if (= amount 0)
                       (incf count)
                       (let ((eligible-containers (remove-if (lambda (el)
                                                              (> el amount))
                                                             containers)))
                         (loop for l = eligible-containers then (cdr l)
                            while l
                            do (let ((container-amount (car l)))
                                 (recur (- amount container-amount)
                                        (cdr l))))))))
      (recur amount containers)
      count)))

(defun read-containers (file)
  (with-open-file (f file)
    (when f
      (loop for line = (read-line f nil nil)
         while line
         collect (parse-integer line)))))

(defun solution-part-1 ()
  (let ((containers (read-containers "no-such-thing-as-too-much-input.txt")))
    (ways-to-store-it 150 containers)))

(defun ways-to-store-it-in-min-containers (amount containers)
  (let ((ways (make-hash-table)))
    (labels ((recur (used amount containers)
               (if (= amount 0)
                   (if (gethash used ways)
                       (incf (gethash used ways))
                       (setf (gethash used ways) 1))
                   (let ((eligible-containers (remove-if (lambda (el)
                                                           (> el amount))
                                                         containers)))
                     (loop for l = eligible-containers then (cdr l)
                        while l
                        do (let ((container-amount (car l)))
                             (recur 
                              (1+ used)
                              (- amount container-amount)
                              (cdr l))))))))
      (recur 0 amount containers)
      (let ((min-used (1- (expt 2 64))))
        (loop for used being the hash-keys of ways 
           do (setf min-used (min min-used used)))
        (cons min-used (gethash min-used ways))))))

(defun solution-part-2 ()
  (let ((containers (read-containers "no-such-thing-as-too-much-input.txt")))
    (ways-to-store-it-in-min-containers 150 containers)))
