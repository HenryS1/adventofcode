(ql:quickload 'cl-ppcre)

(defun spin (programs n)
  (let ((new-programs (make-array (length programs) :element-type 'integer)))
    (loop for i from 0 to (- n 1)
       do (setf (aref new-programs i) 
                (aref programs (- (length programs) (- n i)))))
    (loop for i from n to (- (length programs) 1)
       do (setf (aref new-programs i) 
                (aref programs (- i n))))
    new-programs))

(defun exchange (programs i j)
  (rotatef (aref programs i) (aref programs j))
  programs)

(defun partner (programs a b)
  (exchange programs (position a programs) (position b programs))
  programs)

(defun extract-numbers (str)
  (multiple-value-bind (match registers) 
      (cl-ppcre:scan-to-strings "[^\\d]+(\\d+)[^\\d]+(\\d+)" str)
    (declare (ignore match))
    (map 'list #'parse-integer registers)))

(defun make-exchange (str)
  (destructuring-bind (i j) (extract-numbers str)
    (lambda (programs) (exchange programs i j))))

(defun make-spin (str)
  (let ((n (parse-integer str :start 1)))
    (lambda (programs) (spin programs n))))

(defun make-partner (str)
  (let ((a (aref str 1))
        (b (aref str 3)))
    (lambda (programs) (partner programs a b))))

(defun make-instruction (str)
  (let ((c (aref str 0)))
    (cond ((char= c #\x)
           (make-exchange str))
          ((char= c #\s)
           (make-spin str))
          ((char= c #\p)
           (make-partner str))
          (t (error "unexpected input")))))

(defun parse-input (line)
  (map 'vector #'make-instruction (cl-ppcre:split "," line)))

(defun get-instructions (filename)
  (with-open-file (f filename)
    (when f
      (parse-input (read-line f nil nil)))))

(defun part-1-solver (filename programs-str)
  (let ((programs (map 'vector #'identity programs-str))
        (instructions (get-instructions filename)))
    (loop for instruction across instructions
       do (setf programs (funcall instruction programs)))
    (map 'string #'identity programs)))

(defun find-new-positions (s1 s2)
  (let ((table (make-hash-table)))
    (loop for i from 0 to (- (length s1) 1)
       do (setf (gethash i table) (position (aref s1 i) s2)))
    (let ((new-positions (make-array (hash-table-count table))))
      (loop for k being the hash-keys of table using (hash-value v)
         do (setf (aref new-positions k) v))
      new-positions)))

(defun combine-permutations (p1 p2)
  (apply-permutation p1 p2))

(defun repeat-permutation (p repeats)
  (if (= repeats 0)
      p
      (multiple-value-bind (quotient remainder) (floor repeats 2)
        (let* ((repeated (combine-permutations p p))
               (recurred (repeat-permutation repeated quotient)))
          (if (= remainder 0)
              recurred
              (combine-permutations recurred p))))))

(defun apply-permutation (p vec)
  (map 'vector (lambda (position) (aref vec position)) p))

(defun part-2-solver (filename programs-str repeats)
  (let* ((new-programs-str (part-1-solver filename programs-str))
         (initial-permutation (find-new-positions programs-str new-programs-str))
         (final-permutation (repeat-permutation initial-permutation repeats)))
    (apply-permutation final-permutation (map 'vector #'identity programs-str))))

(defun test-part-1 ()
  (part-1-solver "dancing-programs-test-input" "abcde"))

(defun solution-part-1 ()
  (part-1-solver "dancing-programs-input.txt" "abcdefghijklmnop"))

(defun test-part-2 ()
  (part-2-solver "dancing-programs-test-input" "abcde" 2))

(defun solution-part-2 ()
  (part-2-solver "dancing-programs-input.txt" "abcdefghijklmnop" 1000000000))
