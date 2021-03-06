(defun read-syms (line)
  (let (*read-eval*)
    (with-input-from-string (s line)
      (loop for sym = (read s nil nil)
         while sym collect sym))))

(defun parse-input (line)
  (let ((syms (read-syms (subseq line 0 (- (length line) 1)))))
    (list (car syms) (car (last syms)) (find-if #'numberp syms) 
          (or (find 'lose syms) (find 'gain syms)))))

(defun make-graph (edges)
  (loop with graph = (make-hash-table)
     with nodes = (make-hash-table)
     for (start . rest) in edges
     do (let ((lookup (find-if (lambda (e) (eq (car rest) (car e))) (gethash start graph)))) 
          (if (not lookup)
              (push (if (eq (nth 2 rest) 'gain) 
                        (butlast rest) (list (car rest) (- (cadr rest)))) (gethash start graph))
              (incf (cadr lookup) (if (eq (nth 2 rest) 'gain) (cadr rest) (- (cadr rest))))))
       (let ((lookup (find-if (lambda (e) (eq start (car e))) (gethash (car rest) graph))))
         (if (not lookup)
             (push (list start (if (eq (nth 2 rest) 'gain)
                                   (cadr rest) (- (cadr rest)))) (gethash (car rest) graph))
             (incf (cadr lookup) (if (eq (nth 2 rest) 'gain) (cadr rest) (- (cadr rest))))))
       (setf (gethash start nodes) t)
       (setf (gethash (car rest) nodes) t)
     finally (return (values graph nodes))))

(defun find-best-seating (graph nodes)
  (let (best-value (current-value 0) (seen (make-hash-table)))
    (labels ((rec (start current)
               (if (= (hash-table-count seen) (hash-table-count nodes))
                   (let ((final-edge (find-if (lambda (e) (eq start (car e))) 
                                              (gethash current graph))))
                     (when (or (not best-value) 
                               (> (+ current-value (cadr final-edge)) best-value))
                       (setf best-value (+ current-value (cadr final-edge)))))
                   (loop for (dest weight) in (gethash current graph)
                      when (not (gethash dest seen))
                      do (incf current-value weight)
                        (setf (gethash dest seen) t)
                        (rec start dest)
                        (remhash dest seen)
                        (decf current-value weight)))))
      (loop for k being the hash-keys of graph
         do (setf (gethash k seen) t)
         return (rec k k))
      best-value)))

(defun read-lines ()
  (with-open-file (f "input13")
    (when f (loop for line = (read-line f nil nil) while line collect line))))

(defun part-one ()
  (multiple-value-bind (graph nodes) (make-graph (mapcar #'parse-input (read-lines)))
    (find-best-seating graph nodes)))

(defun add-me-to-graph (graph nodes)
  (loop for person being the hash-keys of nodes 
     do (push (list 'me 0) (gethash person graph))
       (push (list person 0) (gethash 'me graph)))
  (setf (gethash 'me nodes) t))

(defun part-two ()
  (multiple-value-bind (graph nodes) (make-graph (mapcar #'parse-input (read-lines)))
    (add-me-to-graph graph nodes)
    (find-best-seating graph nodes)))
