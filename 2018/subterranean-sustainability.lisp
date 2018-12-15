(ql:quickload :cl-ppcre)

(defun contains-state (line)
  (search "initial" line))

(defun read-current-state (line)
  (remove-if-not (lambda (c) (or (char= c #\.) (char= c #\#))) line))

(defun is-rule (line)
  (search "=>" line))

(defun read-rule (line)
  (cl-ppcre:split "\\s*=>\\s*" line))

(defun read-input ()
  (with-open-file (f "subterranean-sustainability-input.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line
         when (contains-state line)
         collect (read-current-state line)
         when (is-rule line)
         collect (read-rule line)))))

(defun transform-input (inputs)
  (let* ((rules (make-hash-table :test 'equal)))
    (loop for rule-production in (cdr inputs)
       do (setf (gethash (car rule-production) rules) 
                (aref (cadr rule-production) 0)))
    (values rules (cons (map 'list #'identity (car inputs)) 0))))

(let ((one ".") 
      (two "..")
      (three "...")
      (four "...."))
  (defun dots (n)
    (case n
      (1 one)
      (2 two)
      (3 three)
      (4 four))))

(defun make-key (seq)
  (let* ((len (min (length seq) 5))
         (str (coerce (subseq seq 0 len) 'string)))
    (when (< len 5)
      (setf str (concatenate 'string str (dots (- 5 len)))))
    str))

(defun rule-result (rules state)
  (let ((key (make-key state)))
    (gethash key rules)))

(defun set-entry (entry state)
  (if (char= entry #\.)
      (when (cddr state)
        (setf (caddr state) entry))
      (progn 
        (loop for s = state then (cdr s)
           for i from 1 to 2
           when (not (cdr s))
           do (setf (cdr s) (list #\.)))
        (setf (caddr state) entry))))

(defun apply-result (result state)
  (if result
      (set-entry result state)
      (progn 
        (set-entry #\. state))))

(defun trim-state (state-with-index)
  (destructuring-bind (state . index) state-with-index
    (loop for s = state then (cdr s)
       for i = index then (1+ index)
       while (and s (char= (car s) #\.))
       do (setf (car state-with-index) (cdr s))
         (incf (cdr state-with-index)))))

(defun update-state (rules state-with-index)
  (destructuring-bind (state . index) state-with-index
    (let ((new-seq  (copy-seq state))
          (with-prefix (append (list #\. #\. #\. #\.) state))
          (prefix-index (- index 2)))
      (loop for s = with-prefix then (cdr s)
         for pr = (append (list #\. #\. #\. #\.) new-seq) then (cdr pr)
         for i = prefix-index then (+ i 1)
         while s
         do (let ((result (rule-result rules s)))
              (apply-result result pr)
              (when result
                (when (and (< i (cdr state-with-index))
                           (char= result #\#))
                  (setf (cdr state-with-index) i)
                  (setf new-seq (cddr pr))))))
      (setf (car state-with-index) new-seq))
    (trim-state state-with-index)
    state-with-index))

(defun loop-productions (n rules state-with-index)
  (loop for i from 1 to n do (update-state rules state-with-index))
  state-with-index)

(defun test ()
  (multiple-value-bind (rules state-with-index) (transform-input (read-input))
    (let ((result (loop-productions 200 rules state-with-index)))
      result)))

(defun sum-numbers (pots-with-index)
  (let ((pots (car pots-with-index))
        (index (cdr pots-with-index)))
    (loop for p = pots then (cdr p)
       for i = index then (1+ i)
       while p
       when (char= (car p) #\#)
       sum i)))

(defun solution-part-1 ()
  (multiple-value-bind (rules state-with-index) (transform-input (read-input))
    (let ((result (loop-productions 20 rules state-with-index)))
      (sum-numbers result)))) 

(defun solution-part-2 ()
  ;; eventually stabilises and increases by 69 each iteration
  (+ 16068 (* 69 (- 50000000000 200))))
