(load "queue.lisp")

(defun hash-union (one other)
  (let ((new-set (make-hash-table :test 'equal)))
    (loop for k being the hash-keys of one
       do (setf (gethash k new-set) t))
    (loop for k being the hash-keys of other
       do (setf (gethash k new-set) t))
    new-set))

(defun hash-intersection (one other)
  (let ((new-set (make-hash-table :test 'equal)))
    (loop for k being the hash-keys of one
       when (gethash k other)
       do (setf (gethash k new-set) t))
    new-set))

(defun filter-hash-keys (table predicate)
  (let ((new-table (make-hash-table :test 'equal)))
    (loop for k being the hash-keys of table using (hash-value v)
       when (funcall predicate k)
       do (setf (gethash k new-table) v))
    new-table))

(defun filter-hash-values (table predicate)
  (let ((new-table (make-hash-table :test 'equal)))
    (loop for k being the hash-keys of table using (hash-value v)
       when (funcall predicate v)
       do (setf (gethash k new-table) v))
    new-table))

(defun filter-hash-entries (table predicate)
  (let ((new-table (make-hash-table :test 'equal)))
    (loop for k being the hash-keys of table using (hash-value v)
       when (funcall predicate k v)
       do (setf (gethash k new-table) v))
    new-table))

(defun read-lines (filename &optional (tranformation #'identity))
  (with-open-file (file filename)
    (when file
      (loop for line = (read-line file nil nil)
         while line
         collect (funcall tranformation line)))))

(defun print-hash-table (table)
  (loop for k being the hash-keys of table
     using (hash-value v)
     do (format t "~a ~a~%" k v)))

(defun make-set (seq)
  (let ((table (make-hash-table :test 'equal)))
    (if (vectorp seq)
        (loop for el across seq
           do (setf (gethash el table) t))
        (loop for el in seq 
           do (setf (gethash el table) t)))
    table))

(defun hash-keys (table)
  (loop for k being the hash-keys of table collect k))

(defun hash-values (table)
  (loop for v being the hash-values of table collect v))

(defun hash-from-alist (al)
  (let ((table (make-hash-table :test 'equal)))
    (loop for e in al
       do (destructuring-bind (k . v) e
            (setf (gethash k table) v)))
    table))

(defun frequencies (seq)
  (let ((table (make-hash-table :test 'equal)))
    (if (vectorp seq)
        (loop for e across seq
           do (when (not (gethash e table))
                (setf (gethash e table) 0))
             (incf (gethash e table)))
        (loop for e in seq
           do (when (not (gethash e table))
                (setf (gethash e table) 0))
             (incf (gethash e table))))
    table))

(defun make-table (seq make-key)
  (let ((table (make-hash-table :test 'equal)))
    (if (vectorp seq)
        (loop for e across seq
           do (let ((key (funcall make-key e)))
                (when (not (gethash key table))
                  (setf (gethash key table) e))))
        (loop for e in seq
           do (let ((key (funcall make-key e)))
                (when (not (gethash key table))
                  (setf (gethash key table) e)))))
    table))

(defun take (n seq)
  (if (vectorp seq)
      (loop for e across seq
         for i from 1 to n
         collect e)
      (loop for e in seq
         for i from 1 to n
         collect e)))

