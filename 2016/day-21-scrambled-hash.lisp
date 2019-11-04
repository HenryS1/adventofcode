(defun read-instruction (line)
  (let (*read-eval*)
    (with-input-from-string (s line)
      (loop for sym = (read s nil nil)
	    while sym collect sym))))

(defun take (n l)
  (loop for i from 1 to n for e in l collect e))

(defun sym-to-letter (sym) (aref (symbol-name sym) 0))

(defun process-instruction (instruction hash)
  (let ((prefix (take 2 instruction)))
    (cond ((equal prefix '(swap position))
	   (swap (nth 2 instruction) (nth 5 instruction) hash))
	  ((equal prefix '(swap letter))
	   (swap (find (sym-to-letter (nth 2 instruction)) hash)
		 (find (sym-to-letter (nth 5 instruction)) hash)
		 hash))
	  ((equal prefix '(reverse positions))
	   (reverse-positions (nth 2 instruction) (nth 4 instruction) hash))
	  ((equal (car prefix) 'rotate)
	   (rotate (cadr prefix) (nth 2 prefix) hash))
	  ((equal prefix '(move position))
	   (move-position (nth 2 instruction) (nth 5 instruction) hash))
	  ((equal prefix '(rotate based))
	   (rotate 'right (find (sym-to-letter (nth 6 instruction)) hash) hash)))))

(defun swap (i j hash)
  (rotatef (aref hash i) (aref hash j) hash))

(defun reverse-positions (i j hash)
  (loop for start = i then (+ i 1)
	for end = j then (- j 1)
	while (> j i)
	do (swap i j hash)))

(defun rotate (direction n hash)
  ())
