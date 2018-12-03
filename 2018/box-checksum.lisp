(defun read-lines ()
  (with-open-file (f "box-checksum-input.txt")
    (when f 
      (loop for line = (read-line f nil nil)
         while line
         collect line))))

(defun check-char-counts (str)
  (let ((chars (make-hash-table))
        has-two
        has-three)
    (loop for c across str
       do (if (not (gethash c chars))
              (setf (gethash c chars) 0)) 
         (incf (gethash c chars)))
    (loop for v being the hash-values of chars
       do (when (= v 3)
            (setf has-three t))
         (when (= v 2)
           (setf has-two t)))
    (values has-two has-three)))

(defun checksum (strs)
  (let ((twos 0)
        (threes 0))
    (loop for str in strs
       do (multiple-value-bind (has-two has-three) (check-char-counts str)
            (when has-two (incf twos))
            (when has-three (incf threes))))
    (* twos threes)))

(defun char-diff (one other)
  (let ((diff 0))
    (loop for i from 0 to (1- (length one))
       for c across one
       when (not (char= c (aref other i)))
       do (incf diff))
    diff))

(defun find-closest-ids (ids)
  (loop for one in ids
     do (loop for other in ids
           when (= (char-diff one other) 1)
           do (return-from find-closest-ids (cons one other)))))

(defun common-chars (one other)
  (loop for i from 0 to (1- (length one))
     for c across one
     when (not (char= c (aref other i)))
     do (return-from common-chars (concatenate 'string (subseq one 0 i) 
                                               (subseq one (1+ i))))))

(defun solution-part-1 ()
  (checksum (read-lines)))

(defun solution-part-2 ()
  (destructuring-bind (one . other) (find-closest-ids (read-lines))
    (common-chars one other)))
