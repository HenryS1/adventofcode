(ql:quickload :cl-ppcre)

(defun tokenize (line)
  (let ((seperated (cl-ppcre:split ",\\s*" line)))
    (loop for segment in seperated
       collect (create-direction segment))))

(defun create-direction (str)
  (multiple-value-bind (matches subs)
      (cl-ppcre:scan-to-strings "(\\w)(\\d+)" str)
    (declare (ignorable matches))
    (cons (aref subs 0) (parse-integer (aref subs 1)))))

(defun parse-input (file)
  (with-open-file (f file)
      (when f
        (let ((line (read-line f nil nil)))
          (when line
            (tokenize line))))))

(defun turn-right (current-direction)
  (case current-direction 
    (N 'E)
    (E 'S)
    (S 'W)
    (W 'N)))

(defun turn-left (current-direction)
  (case current-direction
    (N 'W)
    (W 'S)
    (S 'E)
    (E 'N)))

(defun next-direction (turn current-direction)
  (if (string-equal turn "R")
      (turn-right current-direction)
      (turn-left current-direction))) 

(defun move (position direction blocks callback)
  (if (= blocks 0)
      position
      (let ((new-position (case direction 
                            (N (cons (+ (car position) 1) (cdr position)))
                            (E (cons (car position) (+ (cdr position) 1)))
                            (S (cons (- (car position) 1) (cdr position)))
                            (W (cons (car position) (- (cdr position) 1))))))
        (funcall callback new-position)
        (move new-position direction (- blocks 1) callback))))

(defun navigate (position direction instructions callback)
  (if (null instructions) 
      position
      (let* ((next-instruction (car instructions))
             (new-direction (next-direction (car next-instruction) direction))
             (next-position (move position new-direction (cdr next-instruction) callback)))
        (navigate next-position new-direction (cdr instructions) callback))))

(defun first-square-visited-twice (instructions)
  (let ((visited (make-hash-table :test 'equal)))
    (setf (gethash '(0 . 0) visited) t)
    (navigate '(0 . 0) 'N instructions 
              (lambda (pos)
                (if (gethash pos visited)
                    (return-from first-square-visited-twice pos)
                    (setf (gethash pos visited) t))))))

(defun follow-instructions (instructions) 
  (navigate '(0 . 0) 'N instructions (lambda (pos) (declare (ignorable pos)))))

(defun manhattan-distance (pos)
  (+ (abs (car pos)) (abs (cdr pos))))

(defun solution-part-1 () 
  (let* ((instructions (parse-input "taxi-cab-input.txt"))
         (destination (follow-instructions instructions)))
    (manhattan-distance destination)))

(defun solution-part-2 ()
  (let* ((instructions (parse-input "taxi-cab-input.txt"))
         (destination (first-square-visited-twice instructions)))
    (manhattan-distance destination)))
