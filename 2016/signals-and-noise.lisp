(defparameter *input-file* "signals-and-noise-input.txt")

(defun read-input (filename)
  (with-open-file (f filename)
    (when f
      (loop for line = (read-line f nil nil)
         while line 
         collect line))))

(defun rank-frequencies (frequencies cmp init-val)
  (loop with mx-freq = init-val
     with best-c = #\$
     for c being the hash-keys of frequencies using (hash-value f)
     when (funcall cmp f mx-freq)
     do (setf mx-freq f)
       (setf best-c c)
     finally (return best-c)))

(defun find-max-frequency (frequencies)
  (rank-frequencies frequencies #'> 0))

(defun find-with-frequency (strs index rank-fun)
  (loop for str in strs
     with frequencies = (make-hash-table)
     for c = (aref str index)
     do (when (not (gethash c frequencies))
          (setf (gethash c frequencies) 0))
       (incf (gethash c frequencies))
     finally (return (funcall rank-fun frequencies))))

(defun find-most-frequent-character (strs index)
  (find-with-frequency strs index #'find-max-frequency))

(defun find-min-frequency (frequencies)
  (rank-frequencies frequencies #'< 99999999))

(defun find-least-frequent-character (strs index)
  (find-with-frequency strs index #'find-min-frequency))

(defun find-password (strs)
  (coerce (loop for i from 0 to (1- (length (car strs)))
             collect (find-most-frequent-character strs i)) 
          'string))

(defun solution-part-1 ()
  (find-password (read-input *input-file*)))

(defun find-uncommon-password (strs)
  (coerce (loop for i from 0 to (1- (length (car strs)))
               collect (find-least-frequent-character strs i))
          'string))

(defun solution-part-2 ()
  (find-uncommon-password (read-input *input-file*)))
