(defun read-input ()
  (with-open-file (f "input10")
    (when f (map 'list #'digit-char-p (read-line f nil nil)))))

(defun look-and-say (ns)
  (loop with count = 1
     for prev = nil then curr
     for rest = ns then (cdr rest)
     for curr = (car rest)
     if (and prev curr (= prev curr))
     do (incf count)
     else if prev
     collect count and
     collect prev and
     do (setf count 1)
     while rest))

(defun loop-and-say (repeats)
  (loop for ns = (read-input) then (look-and-say ns)
     for i from 1 to repeats
     finally (return (length ns))))

(defun part-one ()
  (loop-and-say 40))

(defun part-two ()
  (loop-and-say 50))
