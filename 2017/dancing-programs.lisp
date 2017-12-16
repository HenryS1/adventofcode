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

(defun find-loop (instructions programs-str)
  (let ((table (make-hash-table :test 'equal))
        (len 0))
    (loop until (gethash programs-str table)
       do (progn
            (setf (gethash programs-str table) t)
            (setf programs-str (apply-instructions instructions programs-str))
            (incf len)))
    len))

(defun apply-instructions (instructions programs-str)
  (let ((programs (map 'vector #'identity programs-str)))
    (loop for instruction across instructions
     do (setf programs (funcall instruction programs)))
    (map 'string #'identity programs)))

(defun part-1-solver (filename programs-str)
  (let ((instructions (get-instructions filename)))
    (apply-instructions instructions programs-str)))

(defun find-loop-len ()
  (let ((instructions (get-instructions "dancing-programs-input.txt"))
        (programs-str "abcdefghijklmnop"))
    (find-loop instructions programs-str)))

(defun solution-part-2 ()
  (let* ((instructions (get-instructions "dancing-programs-input.txt"))
         (programs-str "abcdefghijklmnop")
         (loop-len (find-loop instructions programs-str))
         (required-iters (rem 1000000000 loop-len)))
    (labels ((iter (programs-str iters)
               (if (= iters 0)
                   programs-str
                   (iter (apply-instructions instructions programs-str) 
                         (- iters 1)))))
      (iter programs-str required-iters))))

(defun test-part-1 ()
  (part-1-solver "dancing-programs-test-input" "abcde"))

(defun solution-part-1 ()
  (part-1-solver "dancing-programs-input.txt" "abcdefghijklmnop"))

(defun test-part-2 ()
  (part-2-solver "dancing-programs-test-input" "abcde" 2))

(defun solution-part-2 ()
  (part-2-solver "dancing-programs-input.txt" "abcdefghijklmnop" 1000000000))
