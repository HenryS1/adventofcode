(ql:quickload 'cl-ppcre)

(defun parse-input (line)
  (map 'vector #'parse-integer (cl-ppcre:split "," line)))

(defun increment (index vec-len increment)
  (mod (+ index increment) vec-len))

(defun increment-index (index vec increment)
  (increment index (length vec) increment))

(defun reverse-section (current-position len vec)
  (loop for index = current-position then (increment-index index vec 1)
       while (> len 0)
       do (progn 
            (rotatef (aref vec index) 
                     (aref vec (increment-index index vec (- len 1))))
            (decf len 2)))
  vec)

(defun hash (vec lengths current-position skip-size)
  (loop for len across lengths
     do (progn 
          (reverse-section current-position len vec)
          (setf current-position 
                (increment-index current-position vec (+ len skip-size)))
          (incf skip-size)))
  (values vec current-position skip-size))

(defun get-vec ()
  (map 'vector (lambda (n) n) 
       (loop for i from 0 to 255 collect i)))

(defun part-1-solver (vec lengths)
  (let ((final-vec (hash vec lengths 0 0)))
    (* (aref final-vec 0) (aref final-vec 1))))

(defun test-part-1 ()
  (part-1-solver (map 'vector (lambda (n) n) '(0 1 2 3 4))
                 (map 'vector (lambda (n) n) '(3 4 1 5))))

(defun get-lengths ()
  (with-open-file (f "knot-hash-input.txt")
    (parse-input (read-line f nil nil))))

(defun str-to-padded-bytes (str)
  (concatenate 'vector (map 'vector #'char-code str) (vector 17 31 73 47 23)))

(defun input-as-bytes ()
  (with-open-file (f "knot-hash-input.txt")
    (str-to-padded-bytes (read-line f nil nil))))

(defun xor-blocks (vec)
  (let ((result (make-array 16)))
    (loop for i from 0 to 15
       do (progn 
            (setf (aref result i) 
                  (aref vec (* i 16)))
            (loop for j from (+ 1 (* i 16)) to (+ (* i 16) 15)
               do (setf (aref result i)
                        (logxor (aref result i)
                                (aref vec j))))))
    result))

(defun to-hex (numbers)
  (apply #'concatenate 'string
         (loop for num across numbers collect (format nil "~2,'0x" num))))

(defun knot-hash (str)
  (find-knot-hash (get-vec) (str-to-padded-bytes str)))

(defun find-knot-hash (vec lengths)
  (labels ((solve (current-position skip-size rounds-left)
             (if (> rounds-left 0)
                 (multiple-value-bind (vec new-position new-skip-size) 
                     (hash vec lengths current-position skip-size)
                     (declare (ignorable vec))
                     (solve new-position new-skip-size (- rounds-left 1)))
                 vec)))
    (solve 0 0 64)
    (let ((result (xor-blocks vec)))
      (to-hex result))))

;; (defun solution-part-1 ()
;;   (part-1-solver (get-vec) (get-lengths)))

;; (defun solution-part-2 ()
;;   (part-2-solver (get-vec) (input-as-bytes)))
