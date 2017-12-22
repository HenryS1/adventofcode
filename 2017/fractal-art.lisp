(ql:quickload 'cl-ppcre)

(defun apply-pattern (row column pattern grid size)
  (loop for r from row to (+ row size)
     do (loop for c from column to (+ column size)
           do (progn 
                (setf (aref grid r c)
                    (get-entry r c pattern (+ size 1)))))))

(defun get-entry (row column pattern size)
  (aref pattern (+ (* (mod row size) size) (mod column size))))

(defun rotate-3-key (key)
  (destructuring-bind (a b c 
                       d e f
                       g h i) key
    (list g d a h e b i f c)))

(defun rotate-2-key (key)
  (destructuring-bind (a b 
                       c d) key
    (list c a d b)))

(defun rotate-key (key)
  (if (= (length key) 9)
      (rotate-3-key key)
      (rotate-2-key key)))

(defun flip-2-key (key)
  (destructuring-bind (a b 
                       c d) key
    (list b a d c)))

(defun flip-3-key (key)
  (destructuring-bind (a b c
                       d e f
                       g h i) key
    (list c b a f e d i h g)))

(defun flip-key (key)
  (if (= (length key) 9)
      (flip-3-key key)
      (flip-2-key key)))

(defun lookup-pattern (grid row col size patterns)
  (let* ((key (make-key grid row col size))
         (flipped (flip-key key)))
    (loop until (or (gethash key patterns)
                    (gethash flipped patterns))
       do (progn (setf key (rotate-key key))
                 (setf flipped (rotate-key flipped))))
    (or (gethash key patterns)
        (gethash flipped patterns))))

(defun make-key (grid row col size)
  (let (key)
    (loop for r from row to (+ row (- size 1))
       do (loop for c from col to (+ col (- size 1))
             do (push (aref grid r c) key)))
    (reverse key)))

(defun expand-pattern (grid new-grid patterns size)
    (destructuring-bind (rows columns) (array-dimensions grid)
      (loop for r = 0 then (+ r size) 
         while (< r (- rows 1))
         do (loop for c = 0 then (+ c size)
               while (< c (- columns 1))
               do (let* ((pattern (lookup-pattern grid r c size patterns))
                         (new-r (* (+ size 1) (floor r size)))
                         (new-c (* (+ size 1) (floor c size))))
                    (apply-pattern new-r new-c
                                   pattern new-grid 
                                   size))))
      new-grid))

(defun convert-coord (coord size new-size)
  (* (floor coord size) (floor new-size size)))

(defun make-next-grid (r)
  (if (= (mod r 2) 0)
      (let ((dim (+ (floor r 2) r)))
        (make-array (list dim dim) :initial-element #\.))
      (let ((dim (+ (floor r 3) r)))
        (make-array (list dim dim) :initial-element #\.))))

(defun expand-grid (grid patterns)
  (destructuring-bind (r c) (array-dimensions grid)
    (declare (ignorable c))
    (let ((next-grid (make-next-grid r)))
      (if (= (mod r 2) 0)
          (expand-pattern grid next-grid patterns 2)
          (expand-pattern grid next-grid patterns 3)))))

(defun parse-pattern (line)
  (let ((syms (cl-ppcre:split " " line)))
    (list (remove-if (lambda (c) (char= c #\/)) 
                     (loop for c across (car syms) collect c))
          (remove-if (lambda (c) (char= c #\/)) (caddr syms)))))

(defun initial-pattern ()
  (make-array '(3 3) :initial-contents 
              '((#\. #\# #\.)
                (#\. #\. #\#)
                (#\# #\# #\#))))

(defun process-lines (filename callback)
  (with-open-file (f filename)
    (when f
      (loop for line = (read-line f nil nil)
         while line
         do (funcall callback line)))))

(defun get-patterns (filename)
  (let ((patterns (make-hash-table :test 'equal)))
    (process-lines filename (lambda (line)
                     (let ((pattern (parse-pattern line)))
                       (setf (gethash (car pattern) patterns) (cadr pattern)))))
    patterns))

(defun real-patterns ()
  (get-patterns "fractal-art-input.txt"))

(defun test-patterns ()
  (get-patterns "fractal-art-test-input"))

(defun count-turned-on (grid)
  (let ((total 0))
    (destructuring-bind (r c) (array-dimensions grid)
      (declare (ignorable c))
      (loop for i from 0 to (- r 1)
         do (loop for j from 0 to (- r 1)
               when (char= (aref grid i j) #\#)
               do (incf total))))
    total))

(defun expand (iterations patterns)
  (labels ((recur (remaining grid)
             (if (= remaining 0)
                 grid
                 (recur (- remaining 1) (expand-grid grid patterns)))))
    (let ((result (recur iterations (initial-pattern))))
      (count-turned-on result))))
