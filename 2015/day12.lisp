(ql:quickload :yason)

(defun read-input () 
  (with-open-file (f "input12")
    (yason:parse (read-line f nil nil))))

(defun sum-numbers (json)
  (cond ((numberp json) json)
        ((consp json) 
         (loop for e in json
            summing (sum-numbers e)))
        ((hash-table-p json)
         (loop for v being the hash-values of json
            sum (sum-numbers v)))
        (t 0)))

(defun part-one ()
  (sum-numbers (read-input)))

(defun ignore-red (json)
  (cond ((numberp json) json)
        ((consp json)
         (loop for e in json
            summing (ignore-red e)))
        ((hash-table-p json)
         (if (loop for v being the hash-values of json
                when (and (stringp v) (string= v "red"))
                return t)
             0
             (loop for v being the hash-values of json
                sum (ignore-red v))))
        (t 0)))

(defun part-two ()
  (ignore-red (read-input)))
