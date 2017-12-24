(ql:quickload 'cl-ppcre)

(defun create-key (line)
  (multiple-value-bind (match registers) 
      (cl-ppcre:scan-to-strings "(\\d+)/(\\d+)" line)
    (declare (ignore match))
    (let ((parsed (map 'list #'parse-integer registers)))
      (if (> (car parsed) (cadr parsed))
          (cons (cadr parsed) (car parsed))
          (cons (car parsed) (cadr parsed))))))

(defun create-edge (pr)
  (cons pr 1))

(defun edge-strength (edge)
  (+ (caar edge) (cdar edge)))

(defun is-available (edge)
  (> (cdr edge) 0))

(defun incf-available (edge)
  (incf (cdr edge)))

(defun decf-available (edge)
  (decf (cdr edge)))

(defun edges (current graph)
  (gethash current graph))

(defun get-pins (graph)
  (let ((pins (make-hash-table)))
    (loop for edges being the hash-values of graph 
       do (loop for edge in edges
             do (progn 
                  (setf (gethash (caar edge) pins) t)
                  (setf (gethash (cdar edge) pins) t))))
    (loop for k being the hash-keys of pins collect k)))

(defun other (current edge)
  (if (= current (car edge)) 
      (cdr edge)
      (car edge)))

(defun current-strongest (current graph)
  (let ((best-strength-for-component 0))
    (labels ((dfs (current strength trail)
               ;; (format t "current ~a~%" current)
               ;; (format t "strength ~a~%" strength)
               ;; (format t "trail ~a~%" trail)
               (loop for edge in (edges current graph)
                  when (is-available edge)
                  do (progn 
                       (decf-available edge)
                       (let ((total-strength (+ strength (edge-strength edge))))
                         (if (> total-strength best-strength-for-component)
                             (progn 
;                               (format t "strength ~a~%" total-strength)
                               (setf best-strength-for-component total-strength)))
                         (dfs (other current (car edge)) total-strength (cons current trail))
                         (incf-available edge))))))
      (dfs current 0 nil)
      best-strength-for-component)))

(defun find-max-strength (graph)
  (let ((components (get-pins graph))
        (max-strength 0))
    (loop for component in components 
       do (let ((current-strength (current-strongest component graph)))
            (if (> current-strength max-strength)
                (setf max-strength current-strength))))
    max-strength))

(defun process-lines (filename callback)
  (with-open-file (f filename)
    (when f
      (loop for line = (read-line f nil nil)
         while line
         do (funcall callback line)))))

(defun build-graph (filename)
  (let ((graph (make-hash-table :test 'equal))
        (edges (make-hash-table :test 'equal)))
    (process-lines filename 
                   (lambda (line)
                     (let* ((key (create-key line))
                            (edge (gethash key edges)))
                       (if edge
                           (incf-available edge)
                           (let ((new-edge (create-edge key)))
                             (push new-edge (gethash (car key) graph))
                             (push new-edge (gethash (cdr key) graph))
                             (setf (gethash key edges) new-edge))))))
    graph))

(defun real-graph ()
  (build-graph "electromagnetic-moat-input.txt"))

(defun test-graph ()
  (build-graph "electromagnetic-moat-test-input"))

