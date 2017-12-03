(let ((max-short 65535))
  (defun compute-value (name circuit)
    (labels ((find-value (depends)
               (case (car depends)
                 (val (compute-value (cdr depends) circuit))
                 (and-gate (logand (compute-value (cadr depends) circuit)
                                   (compute-value (cddr depends) circuit)))
                 (or-gate (logior (compute-value (cadr depends) circuit)
                                  (compute-value (cddr depends) circuit)))
                 (not-gate (logand max-short
                                   (lognot (compute-value (cdr depends) circuit))))
                 (lshift (logand max-short 
                                 (ash (compute-value (cadr depends) circuit) 
                                      (cddr depends))))
                 (rshift (logand max-short 
                                 (ash (compute-value (cadr depends) circuit) 
                                      (- (cddr depends)))))
                 (otherwise 0))))
      (if (numberp name)
          name
          (let ((value (find-value (gethash name circuit))))
            (setf (gethash name circuit) (cons 'val value))
            value)))))

(defun add-to-circuit (name depends circuit)
  (setf (gethash name circuit) depends))

(defun make-body (syms)
  (cond ((equal (car syms) 'not)
         (cons 'not-gate (cadr syms)))
        ((equal (cadr syms) '->)
         (cons 'val (car syms)))
        ((or (equal (cadr syms) 'rshift)
             (equal (cadr syms) 'lshift))
         (cons (cadr syms) (cons (car syms) (caddr syms))))
        ((equal (cadr syms) 'and)
         (cons 'and-gate (cons (car syms) (caddr syms))))
        ((equal (cadr syms) 'or)
         (cons 'or-gate (cons (car syms) (caddr syms))))))

(defun make-gate (syms)
  (let ((name (car (last syms)))
        (body (make-body syms)))
    (values name body)))

(defun get-syms (line)
  (let (*read-eval*) 
    (with-input-from-string (s line)
      (loop for sym = (read s nil nil)
         while sym
         collect sym))))

(defun parse-input (line)
  (make-gate (get-syms line)))

(defun process-lines (filename callback)
  (with-open-file (f filename)
    (loop for line = (read-line f nil nil)
       while line
       do (funcall callback line))))

(defun compute-values (circuit)
  (loop for k being the hash-keys of circuit
     do (format t "~a -> ~a~%" k (compute-value k circuit))))

(defun construct-circuit (filename)
  (let ((circuit (make-hash-table :test 'equal)))
    (process-lines filename
     (lambda (line) 
       (multiple-value-bind (name body) (parse-input line)
         (add-to-circuit name body circuit))))
    circuit))

(defun solution-to-part-1 ()
  (let ((circuit (construct-circuit "assembly-required-input.txt")))
    (compute-value 'a circuit)))

(defun solution-to-part-2 ()
  (let ((circuit (construct-circuit "assembly-required-input.txt")))
    (let ((a (compute-value 'a circuit)))
      (let ((new-circuit (construct-circuit "assembly-required-input.txt")))
        (setf (gethash 'b new-circuit) (cons 'val a))
        (compute-value 'a new-circuit)))))
