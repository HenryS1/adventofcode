(defun make-pq (cmp &rest contents)
  (let ((pq (list (make-array 5 :adjustable t :fill-pointer 1) 0 cmp)))
    (loop for e in contents
       do (insert-pq e pq))
    pq))

(defun pq-nonempty (pq)
  (> (cadr pq) 0))

(defun pq-empty (pq)
  (= (cadr pq) 0))

(defun pq-size (pq)
  (cadr pq))

(defun bubble-up (q index cmp)
  (loop while (and (> index 1)
                   (funcall cmp
                            (aref q index)
                            (aref q (floor index 2))))
     do (rotatef (aref q index) (aref q (floor index 2)))
       (setf index (floor index 2))))

(defun insert-pq (el pq)
  (destructuring-bind (q size cmp) pq
    (vector-push-extend el q (length q))
    (incf size)
    (setf (cadr pq) size)
    (bubble-up q size cmp)))

(defun validate (q index size cmp)
  (if (> index size)
      t
      (and (if (< (* index 2) size)
               (progn 
                 (assert (not (funcall cmp (aref q (* index 2)) (aref q index))))
                 (not (funcall cmp (aref q (* index 2)) (aref q index))))
               t)
           (if (< (1+ (* index 2)) size)
               (progn 
                 (assert (not (funcall cmp (aref q (1+ (* index 2))) (aref q index))))
                 (not (funcall cmp (aref q (1+ (* index 2))) (aref q index))))
               t)
           (validate q (* 2 index) size cmp)
           (validate q (1+ (* 2 index)) size cmp))))

(defun sink (q size cmp)
  (loop with index = 1
     with el = (aref q index)
     with child = index
     do (when (and (<= (* 2 index) size)
                   (funcall cmp (aref q (* 2 index)) el))
          (setf child (* 2 index)))
       (when (and (<= (1+ (* 2 index)) size)
                  (funcall cmp (aref q (1+ (* 2 index))) (aref q child)))
         (setf child (1+ (* 2 index))))
     while (/= child index)
     do (rotatef (aref q index) (aref q child))
       (setf index child)))

(defun pop-pq (pq)
  (destructuring-bind (q size cmp) pq
    (if (= size 0)
        nil
        (let ((top (aref q 1)))
          (rotatef (aref q 1) (aref q size))
          (vector-pop q)
          (decf (cadr pq))
          (decf size)
          (sink q size cmp)
          top))))
