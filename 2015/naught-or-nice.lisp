(ql:quickload 'cl-ppcre)

(let ((vowels "aeiou"))
  (defun at-least-three-vowels (str)
    (labels ((is-vowel (c) (find c vowels))
             (vowel-count () 
               (reduce (lambda (total c) 
                         (if (is-vowel c) (+ 1 total) total)) str :initial-value 0)))
      (>= (vowel-count) 3))))

(defun has-letter-twice-in-a-row (str)
  (let (current)
    (loop for c across str
       do (if (and current (char= current c))
              (return-from has-letter-twice-in-a-row t)
              (setf current c)))
    nil))

(defun doesnt-have-banned (str)
  (not (cl-ppcre:scan "(ab|cd|pq|xy)" str)))

(defun is-nice (str)
  (and (at-least-three-vowels str)
       (has-letter-twice-in-a-row str)
       (doesnt-have-banned str)))

(defun process-lines (callback)
  (with-open-file (f "naughty-or-nice-input.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line
         do (funcall callback line)))))

(defun two-letter-repeat-no-overlap (str)
  (cl-ppcre:scan "(\\w{2}).*\\1" str))

(defun one-letter-repeat-with-one-letter-between (str)
  (cl-ppcre:scan "(\\w).\\1" str))

(defun revised-is-nice (str)
  (and (two-letter-repeat-no-overlap str)
       (one-letter-repeat-with-one-letter-between str)))

(defun solution-part-1 ()
  (let ((nice-string-count 0))
    (process-lines (lambda (line)
                     (if (is-nice line)
                         (incf nice-string-count))))
    nice-string-count))

(defun solution-part-2 ()
  (let ((nice-string-count 0))
    (process-lines (lambda (line)
                     (if (revised-is-nice line)
                         (incf nice-string-count))))
    nice-string-count))
