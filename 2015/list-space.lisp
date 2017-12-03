(defun enclosing-quote (str position)
  (char= (aref str position) #\"))

(defun quote-step (str position)
  (and 
   (> (- (length str) position) 1)
   (char= (aref str position) #\\)
   (char= (aref str (+ position 1))  #\")))

(defun slash-step (str position)
  (and 
   (> (- (length str) position) 1)
   (char= (aref str position) #\\)
   (char= (aref str (+ position 1)) #\\)))

(let ((hex-chars "0123456789abcdef"))
  (defun is-hex (c)
    (find c hex-chars)))

(defun hex-step (str position)
    (and 
     (> (- (length str) position 3))
     (char= (aref str position) #\\)
     (char= (aref str (+ position 1)) #\x)
     (is-hex (aref str (+ position 2)))
     (is-hex (aref str (+ position 3)))))

(defun is-newline (c)
  (or (char= c #\Newline)
      (char= c #\Return)))

(defun is-whitespace (c)
  (or (is-newline c)
      (char= c #\Space)
      (char= c #\Tab)))

(defun read-whitespace (strm)
  (if (and (peek-char t strm nil nil)
           (is-whitespace (peek-char t strm nil nil)))
      (loop for c = (read-char strm nil nil)
         while (and c (is-whitespace c))
         until (and (peek-char strm nil nil)
                    (not (is-whitespace (peek-char strm nil nil)))))))

(defun read-quoted-line (strm)
  (progn 
    (read-whitespace strm)
    (let ((chars (loop for c = (read-char strm nil nil)
                    while (and c (not (is-newline c)))
                    collect c)))
      (if chars
          (coerce chars 'vector)
          chars))))

(defun count-tokens (line)
  (labels
      ((recur (str position count)
         (cond 
           ((= (length str) position) count)
           ((quote-step str position)
            (recur str (+ position 2) (+ count 1)))
           ((slash-step str position)
            (recur str (+ position 2) (+ count 1)))
           ((hex-step str position)
            (recur str (+ position 4) (+ count 1)))
           ((enclosing-quote str position)
            (recur str (+ position 1) count))
           (t (recur str (+ position 1) (+ count 1))))))
    (recur line 0 0)))

(defun process-lines (filename callback)
  (with-open-file (f filename)
    (when f
      (loop for line = (read-quoted-line f)
         while line
         do (funcall callback line)))))

(defun total-difference (filename)
  (let ((total 0))
    (process-lines filename 
                 (lambda (line)
                   (incf total (- (length line) (count-tokens line)))))
    total))

(defun encoded-length (line)
  (labels 
      ((recur (str position count)
         (cond 
           ((= (length str) position) count)
           ((quote-step str position)
            (recur str (+ position 2) (+ count 4)))
           ((slash-step str position)
            (recur str (+ position 2) (+ count 4)))
           ((hex-step str position)
            (recur str (+ position 4) (+ count 5)))
           ((enclosing-quote str position)
            (recur str (+ position 1) (+ count 3)))
           (t (recur str (+ position 1) (+ count 1))))))
    (recur line 0 0)))

(defun total-encoded-difference (filename)
  (let ((total 0))
    (process-lines filename
                   (lambda (line)
                     (incf total (- (encoded-length line) (length line)))))
    total))

(defun test-input-1 ()
  (total-difference "list-space-test-input.txt"))

(defun test-input-2 ()
  (total-encoded-difference "list-space-test-input.txt"))

(defun solution-part-1 ()
  (total-difference "list-space-input.txt"))

(defun solution-part-2 ()
  (total-encoded-difference "list-space-input.txt"))
