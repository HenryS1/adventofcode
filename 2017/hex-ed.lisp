(ql:quickload 'cl-ppcre)

(defun parse-input (line)
  (mapcar (lambda (w) (intern (string-upcase w))) 
          (cl-ppcre:split "," line)))

(defun add-move (move move-counts increment)
  (if (gethash move move-counts)
      (incf (gethash move move-counts) increment)
      (setf (gethash move move-counts) increment)))

(defun cancel (one other move-counts)
  (let ((one-count (gethash one move-counts))
        (other-count (gethash other move-counts)))
    (if (and one-count other-count)
        (progn 
          (if (> one-count other-count)
              (progn (remhash other move-counts)
                     (setf (gethash one move-counts) 
                           (- one-count other-count))
                     (if (= 0 (gethash one move-counts))
                         (remhash one move-counts)))
              (progn (remhash one move-counts)
                     (setf (gethash other move-counts)
                           (- other-count one-count))
                     (if (= 0 (gethash other move-counts))
                         (remhash other move-counts))))
          t)
        nil)))

(defun simplify-opposites (move-counts)
  (progn 
    (cancel 'N 'S move-counts)
    (cancel 'E 'W move-counts)
    (cancel 'NE 'SW move-counts)
    (cancel 'NW 'SE move-counts)))

(defun make-move (one other)
  (cond ((and (eq one 'NE) (eq other 'S)) 'SE)
        ((and (eq one 'NW) (eq other 'S)) 'SW)
        ((and (eq one 'SE) (eq other 'N)) 'NE)
        ((and (eq one 'SW) (eq other 'N)) 'NW)
        ((and (eq one 'SE) (eq other 'SW)) 'S)
        ((and (eq one 'NE) (eq other 'NW)) 'N)
        (t nil)))

(defun combine-move (one other move-counts)
  (let* ((one-count (gethash one move-counts))
         (other-count (gethash other move-counts))
         (new-move (make-move one other)))
    (if (and new-move one-count other-count)
        (progn
          (if (> one-count other-count)
              (progn 
                (remhash other move-counts)
                (setf (gethash one move-counts)
                      (- one-count other-count))
                (add-move new-move move-counts other-count)
                (if (= 0 (gethash one move-counts))
                    (remhash one move-counts)))
              (progn (remhash one move-counts)
                     (setf (gethash other move-counts)
                           (- other-count one-count))
                     (add-move new-move move-counts one-count)
                     (if (= 0 (gethash other move-counts))
                         (remhash other move-counts))))
          t)
        nil)))

(defun combine-moves (move-counts)
  (progn 
    (combine-move 'NE 'S move-counts)
    (combine-move 'NW 'S move-counts)
    (combine-move 'SE 'N move-counts)
    (combine-move 'SW 'N move-counts)
    (combine-move 'SE 'SW move-counts)
    (combine-move 'NE 'NW move-counts)))

(defun remove-zero-counts (move-counts)
  (let ((keys (loop for k being the hash-keys of move-counts collect k)))
    (loop for key in keys
       when (= (gethash key move-counts) 0)
       do (remhash key move-counts))))

(defun simplify (move-counts)
  (loop while (or 
               (simplify-opposites move-counts)
               (combine-moves move-counts)))
  (remove-zero-counts move-counts))

(defun initial-table (input)
  (let ((move-counts (make-hash-table)))
    (loop for move in input
       do (add-move move move-counts 1))
    move-counts))

(defun navigate (input)
  (let ((move-counts (initial-table input)))
    (simplify move-counts)
    move-counts))

(defun navigate-step (input callback)
  (let ((move-counts (make-hash-table)))
    (loop for move in input 
       do (progn 
            (add-move move move-counts 1)
            (simplify move-counts)
            (funcall callback move-counts)))))

(defun get-input ()
  (with-open-file (f "hex-ed-input.txt")
    (when f 
      (read-line f nil nil))))

(defun sum-moves (move-counts)
  (let ((total 0))
    (loop for v being the hash-values of move-counts
       do (incf total v))
    total))

(defun part-1-solver (str)
  (let ((input (parse-input str)))
    (sum-moves (navigate input))))

(defun part-2-solver (str)
  (let ((input (parse-input str))
        (max-distance 0))
    (navigate-step input (lambda (move-counts)
                           (let ((new-count (sum-moves move-counts)))
                             (if (> new-count max-distance)
                                 (setf max-distance new-count)))))
    max-distance))

(defun solution-part-1 ()
  (part-1-solver (get-input)))

(defun solution-part-2 ()
  (part-2-solver (get-input)))
