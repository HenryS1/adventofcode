(defun generate-data (a)
  (let ((b (nreverse (copy-seq a))))
    (loop for i from 0 to (- (length b) 1)
       do (if (= (aref b i) 0)
              (setf (aref b i) 1)
              (setf (aref b i) 0)))
    (concatenate 'bit-vector a #*0 b)))

(defun checksum (v)
  (loop with result = (make-array (floor (length v) 2) :element-type 'bit)
     for fst = 0 then (+ fst 2) 
     for snd = 1 then (+ snd 2)
     while (< snd (length v))
     when (= (aref v fst) (aref v snd))
     do (setf (aref result (floor fst 2)) 1)
     when (/= (aref v fst) (aref v snd))
     do (setf (aref result (floor fst 2)) 0)
     finally (return result)))

(defun fill-disk (init size)
  (loop with data = init
     while (< (length data) size)
     do (setf data (generate-data data))
     finally (return data)))

(defun find-checksum (init size)
  (loop with chk = (subseq init 0 size)
     while (evenp (length chk))
     do (setf chk (checksum chk))
     finally (return chk)))

(defun find-disk-checksum (init size)
  (find-checksum (fill-disk init size) size))

(defun answer1 ()
  (find-disk-checksum #*11011110011011101 272))

(defun answer2 ()
  (find-disk-checksum #*11011110011011101 35651584))
