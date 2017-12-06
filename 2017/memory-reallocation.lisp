(defun find-max-block (banks)
  (let (mx
        (index 0))
    (loop for bank across banks
       do (progn 
            (if (or (not mx) 
                    (> bank (car mx))
                    (and (= bank (car mx))
                         (< index (cdr mx))))
                (setf mx (cons bank index)))
            (incf index)))
    mx))

(defun increment-index (index banks)
  (mod (+ index 1) (length banks)))

(defun reallocate-max (mx banks)
  (let ((index (cdr mx))
        (val (car mx)))
    (setf (aref banks index) 0)
    (loop for i = (increment-index index banks) then (increment-index i banks)
       while (> val 0)
       do (progn 
            (incf (aref banks i))
            (decf val)))
    banks))

(defun reallocate (banks)
  (let ((mx (find-max-block banks)))
    (reallocate-max mx banks)))

(defun make-key (banks)
  (map 'list (lambda (x) x) banks))

(defun find-already-seen (banks callback)
  (let ((table (make-hash-table :test 'equal)))
    (loop for b = banks then (reallocate banks)
       do (let ((key (make-key banks)))
            (if (gethash key table)
                (return-from find-already-seen banks)
                (progn 
                  (funcall callback)
                  (setf (gethash key table) t)))))))

(defun count-to-first-repeat (banks)
  (let ((cycles 0))
    (find-already-seen banks (lambda () (incf cycles)))
    cycles))

(defun count-from-first-repeat-to-next (banks)
  (let ((table (make-hash-table :test 'equal))
        (count 0)
        first-repeat)
    (loop for b = banks then (reallocate banks)
       do (let ((key (make-key banks)))
            (if (equal first-repeat key)
                (return-from count-from-first-repeat-to-next count))
            (if (gethash key table)
                (if (not first-repeat)
                    (setf first-repeat key)))
            (setf (gethash key table) t)
            (if first-repeat
                (incf count))))))

(defun solution-part-2 ()
  (count-from-first-repeat-to-next #(0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11)))

(defun solution-part-1 ()
  (let ((input #(0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11)))
    (count-to-first-repeat input)))
