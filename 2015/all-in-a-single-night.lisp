(defun add-edge (source edge graph vertices)
  (progn 
    (push edge (gethash source graph))
    (push (cons source (cdr edge)) (gethash (car edge) graph))
    (setf (gethash source vertices) t)
    (setf (gethash (car edge) vertices) t)))

(defun parse-input (line)
  (let (*read-eval*)
    (with-input-from-string (s line)
      (loop for sym = (read s nil nil)
         while sym collect sym))))

(defun make-edge (syms)
  (let ((source (car syms))
        (edge (cons (caddr syms) 
                    (cadddr (cdr syms)))))
    (values source edge)))

(defun process-lines (filename callback)
  (with-open-file (f filename)
    (when f 
      (loop for line = (read-line f nil nil)
         while line
         do (funcall callback line)))))

(defun collect-vertices (vertices)
  (loop for k being the hash-keys of vertices collect k))

(defun make-graph (filename)
  (let ((graph (make-hash-table :test 'equal))
        (vertices (make-hash-table)))
  (process-lines filename (lambda (line)
                     (multiple-value-bind (source edge) (make-edge (parse-input line))
                       (add-edge source edge graph vertices))))
    (values (collect-vertices vertices) graph)))

(defun has-edge (one other graph)
  (gethash (cons one other) graph))

(defun visited-all-vertices (visited total-vertices)
  (= (hash-table-count visited) total-vertices))

(defun best-path (start graph total-vertices decision-function)
  (labels 
      ((recur (start visited distance)
         (if (visited-all-vertices visited total-vertices)
             distance
             (let (best-distance)
               (loop for edge in (gethash start graph)
                  when (not (gethash (car edge) visited))
                  do (let ((vertex (car edge)))
                       (setf (gethash vertex visited) t)
                       (let ((total-distance (recur vertex visited (+ distance (cdr edge)))))
                         (if (or (not best-distance) 
                                 (and total-distance 
                                      (funcall decision-function total-distance best-distance)))
                             (setf best-distance total-distance)))
                       (remhash vertex visited)))
               best-distance))))
    (let ((visited (make-hash-table :test 'equal)))
      (setf (gethash start visited) t)
      (let ((result (recur start visited 0)))
        result))))

(defun hamiltonian-path (graph vertices decision-function)
  (let ((total-vertices (length vertices)))
    (labels ((find-distance ()
               (let (best-distance)
                 (loop for vertex in vertices
                    do (let ((distance 
                              (best-path vertex graph total-vertices decision-function)))
                         (if (or (not best-distance)
                                 (and distance (funcall decision-function distance best-distance)))
                             (setf best-distance distance))))
                 (or best-distance 0))))
      (find-distance))))

(defun solve (filename decision-function)
  (multiple-value-bind (vertices graph) (make-graph filename)
    (hamiltonian-path graph vertices decision-function)))

(defun test-input-1 ()
  (solve "all-in-a-night-test-input.txt" #'<))

(defun solution-part-1 ()
  (solve "all-in-a-single-night-input.txt" #'<))

(defun test-input-2 ()
  (solve "all-in-a-night-test-input.txt" #'>))

(defun solution-part-2 ()
  (solve "all-in-a-single-night-input.txt" #'>))
