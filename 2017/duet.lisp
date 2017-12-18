(defun make-q ()
  (list nil nil))

(defun enqueue (el q)
  (progn 
    (if (null (car q))
      (progn (setf (car q) (list el))
             (setf (cadr q) (car q)))
      (let ((end (list el)))
        (setf (cdadr q) end)
        (setf (cadr q) end)))
    q))

(defun empty-q (q)
  (null (car q)))

(defun dequeue (q)
  (if (null (car q))
      nil
      (let ((fst (caar q)))
        (setf (car q) (cdar q))
        fst)))

(defun snd-reg (x registers snd-callback)
  (funcall snd-callback (get-value x registers)))

(defun get-value (x registers)
  (if (numberp x)
      x
      (let ((lookup (gethash x registers)))
        (if lookup
            lookup 
            0))))

(defun set-reg (x y registers)
;  (format t "set: ~a ~a~%" x (get-value y registers))
  (setf (gethash x registers) 
        (get-value y registers))
;  (format t "set value is ~a~%" (gethash x registers))
  )

(defun apply-reg-op (x y registers binop)
  (setf (gethash x registers)
        (funcall binop 
                 (get-value x registers)
                 (get-value y registers))))

(defun add-reg (x y registers)
  (apply-reg-op x y registers #'+))

(defun mul-reg (x y registers)
  (apply-reg-op x y registers #'*))

(defun mod-reg (x y registers)
  (apply-reg-op x y registers #'mod))

(defun rcv-reg (x registers rcv-callback)
  (funcall rcv-callback x registers))

(defun jgz-reg (x y registers)
;  (format t "jmp ~a ~a~%" x y)
;  (format t "reg value ~a~%" (get-value x registers))
  (if (> (get-value x registers) 0)
      (get-value y registers)
      nil))

(defun read-instruction (line)
  (let (*read-eval*)
    (with-input-from-string (in line)
      (loop for sym = (read in nil nil)
         while sym collect sym))))

(defun collect-instructions (filename)
  (map 'vector #'identity
       (with-open-file (f filename)
         (when f
           (loop for line = (read-line f nil nil)
              while line
              collect (read-instruction line))))))

(defun test-instructions ()
  (collect-instructions "duet-test-2-input"))

(defun interpret-next (instruction instruction-index registers snd-callback rcv-callback)
  (let ((next-index (+ instruction-index 1)))
;    (format t "instruction ~a~%" instruction)
    (case (car instruction)
        (snd (snd-reg (cadr instruction) registers snd-callback))
        (set (set-reg (cadr instruction) (caddr instruction) registers))
        (add (add-reg (cadr instruction) (caddr instruction) registers))
        (mul (mul-reg (cadr instruction) (caddr instruction) registers))
        (mod (mod-reg (cadr instruction) (caddr instruction) registers))
        (rcv (let ((jmp (rcv-reg (cadr instruction) registers rcv-callback)))
               (setf next-index (+ instruction-index jmp))))
        (jgz (let ((jmp (jgz-reg (cadr instruction) (caddr instruction) registers)))
               (if jmp
                   (progn 
;                     (format t "jumping ~a~%" jmp)
                     (setf next-index (+ instruction-index jmp))))))
        (otherwise (error "unexpected instruction")))
    next-index))

(defun deadlocked (core1 core2 instructions)
  (multiple-value-bind (consume-instruction1 is-waiting1) (funcall core1)
    (declare (ignorable consume-instruction1))
    (multiple-value-bind (consume-instruction2 is-waiting2) (funcall core2)
      (declare (ignore consume-instruction2))
      (let ((locked 
             (or (and 
                  (not (terminated-core core1 instructions))
                  (not (terminated-core core2 instructions))
                  (funcall is-waiting1 instructions)
                  (funcall is-waiting2 instructions))
                 (and (terminated-core core1 instructions)
                      (not (terminated-core core2 instructions))
                      (funcall is-waiting2 instructions))
                 (and (not (terminated-core core1 instructions))
                      (funcall is-waiting1 instructions)
                      (terminated-core core2 instructions)))))
        (if locked 
            (format t "deadlocked~%"))
        locked))))

(defun terminated-core (core instructions)
  (multiple-value-bind (consume waiting sent terminated) (funcall core)
    (declare (ignorable consume waiting sent))
    (funcall terminated instructions)))

(defun both-terminated (core1 core2 instructions)
  (or (deadlocked core1 core2 instructions)
      (and (terminated-core core1 instructions) 
           (terminated-core core2 instructions))))

(defun core (id rcv-q snd-q)
  (let ((registers (make-hash-table))
        (index 0)
        (sent 0))
    (setf (gethash 'p registers) id)
    (labels ((snd-val (val)
               (progn 
                 (incf sent)
                 (enqueue val snd-q)))
             (rcv-val (x registers)
               (let ((dequeued (dequeue rcv-q)))
                 (if dequeued
                     (progn 
                       (set-reg x dequeued registers)
                       1)
                     0)))
             (terminated (instructions)
               (or (< index 0) (>= index (length instructions))))
             (is-waiting (instructions)
               (and (eq (car (aref instructions index)) 'rcv)
                    (empty-q rcv-q)))
             (consume-instruction (instructions)
;               (format t "id ~a~%" id)
               (if (not (terminated instructions))
                   (setf index (interpret-next (aref instructions index) index
                                           registers #'snd-val #'rcv-val))))
             (get-sent ()
               sent))
      (lambda ()
        (values #'consume-instruction #'is-waiting #'get-sent #'terminated)))))

(defun part-2-solver (instructions)
  (let* ((q1 (make-q))
         (q2 (make-q))
         (core1 (core 0 q1 q2))
         (core2 (core 1 q2 q1)))
    (labels ((recur ()
               (if (both-terminated core1 core2 instructions)
                   (multiple-value-bind (consume-instruction is-waiting get-sent) (funcall core2)
                     (declare (ignorable consume-instruction is-waiting))
                     (funcall get-sent))
                   (multiple-value-bind (consume-instruction1) (funcall core1)
                     (multiple-value-bind (consume-instruction2) (funcall core2)
                       (funcall consume-instruction1 instructions)
                       (funcall consume-instruction2 instructions)
                       (recur))))))
      (recur))))

(defun real-instructions ()
  (collect-instructions "duet-input.txt"))

(defun test-part-2 ()
  (part-2-solver (test-instructions)))

(defun solution-part-2 ()
  (part-2-solver (real-instructions)))
