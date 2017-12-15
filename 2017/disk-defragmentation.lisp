(load "knot-hash.lisp")

(defun create-row (input row-number)
  (let ((hash-input (format nil "~a-~a" input row-number)))
    (knot-hash hash-input)))

(defun hex-to-num (cs start end)
  (parse-integer cs :start start :end end :radix 16))

(defun bits-in-num (b)
  (if (= b 0)
      0
      (+ (mod b 2) 
         (bits-in-num (floor b 2)))))

(defun collect-bits (row)
  (apply #'append (loop for c across row collect 
               (collect-bits-in-num (parse-integer (string c) :radix 16)))))

(defun collect-bits-in-num (b)
  (labels ((recur (b acc remaining)
             (if (= remaining 0)
                 acc
                 (recur (floor b 2) (cons (mod b 2) acc) (- remaining 1)))))
    (recur b nil 4)))

(defun count-used-bits (cs start)
  (bits-in-num (hex-to-num cs start (+ start 1))))

(defun alternative-count (row)
  (let ((total 0))
    (loop for start from 0 to (- (length row) 1)
       do (incf total (count-used-bits row start)))
    total))

(defun count-used-in-row (row)
  (labels ((recur (start total)
             (if (> start (- (length row) 1))
                 total
                 (let ((new-total (+ total (count-used-bits row start))))
                   (recur (+ start 1) new-total)))))
    (recur 0 0)))

(defun add-to-graph (row-num row-digits graph)
  (let ((index 0))
    (loop for b in row-digits 
       do (progn 
            (if (= b 1)
                (progn 
                  (setf (gethash (cons row-num index) graph) t)))
            (incf index)))))

(defun build-graph (input)
  (let ((graph (make-hash-table :test 'equal)))
    (loop for i from 0 to 127 
       do (let ((row-bits (collect-bits (create-row input i))))
            (add-to-graph i row-bits graph)))
    graph))

(defun neighbours (cell)
  (list (cons (car cell) (+ (cdr cell) 1))
        (cons (car cell) (- (cdr cell) 1))
        (cons (+ (car cell) 1) (cdr cell))
        (cons (- (car cell) 1) (cdr cell))))

(defun connected-components (graph)
  (let ((seen (make-hash-table :test 'equal))
        (components 0))
    (labels ((dfs (cell)
               (loop for neighbour in (neighbours cell)
                  when (and (gethash neighbour graph)
                            (not (gethash neighbour seen)))
                  do (progn 
                       (setf (gethash neighbour seen) t)
                       (dfs neighbour))))
             (count-components ()
               (loop for i from 0 to 127 
                  do (loop for j from 0 to 127
                        when (let ((cell (cons i j)))
                               (and (gethash cell graph)
                                    (not (gethash cell seen))))
                        do (let ((cell (cons i j)))
                             (setf (gethash cell seen) t)
                             (incf components)
                             (dfs cell))))
               components))
      (count-components))))

(defun part-1-solver (input)
  (let ((total 0))
    (loop for i from 0 to 127
       do (let ((hsh (collect-bits (create-row input i))))
            (incf total (reduce #'+ hsh))))
    total))

(defun part-2-solver (input)
  (let ((graph (build-graph input)))
    (connected-components graph)))

(defun test-part-1 ()
  (part-1-solver "flqrgnkx"))

(defun test-part-2 ()
  (part-2-solver "flqrgnkx"))

(defun solution-part-1 ()
  (part-1-solver "amgozmfv"))

(defun solution-part-2 ()
  (part-2-solver "amgozmfv"))
