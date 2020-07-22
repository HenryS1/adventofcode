(ql:quickload :cl-ppcre)

(defun count-escaped (line)
  (loop for i from 1 to (- (length line) 1)
     for c = (aref line i)
     for prev = (aref line (- i 1))
     when (and (char= prev #\\) (or (char= c #\\) (char= c #\")))
     count c and
     do (incf i 1)))

(defun count-hex-codes (line)
  (let ((hexes "0123456789abcdef"))
    (loop for i  from 1 to (- (length line) 1)
       for c = (aref line i)
       for cp = (aref line (- i 1))
       for cpp = (if (>= i 2) (aref line (- i 2)) nil)
       for cppp = (if (>= i 3) (aref line (- i 3)) nil)
       if (and cppp cpp
                 (char= cppp #\\) (char= cpp #\x)
                 (find cp hexes)
                 (find c hexes))
       count c and
       do (incf i 1))))

(defun extra-chars (line)
  (+ (* 3 (count-hex-codes line))
     (count-escaped line)
     2))

(defun part-one ()
  (with-open-file (f "input8")
    (when f
      (loop for line = (read-line f nil nil)
         while line 
         sum (extra-chars line)))))

(defun count-slash-or-quote (line)
  (count-if (lambda (c) (or (char= c #\\) (char= c #\"))) line))

(defun part-two ()
  (with-open-file (f "input8")
    (when f
      (loop for line = (read-line f nil nil)
         while line
         sum (+ 2 (count-slash-or-quote line))))))
