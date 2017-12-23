(defun get-value (x registers)
  (if (numberp x)
      x
      (let ((lookup (gethash x registers)))
        (if lookup lookup 0))))

(defun set-reg (x y registers)
  (setf (gethash x registers) 
        (get-value y registers)))

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

(defun sub-reg (x y registers)
  (apply-reg-op x y registers #'-))

(defun jez-reg (x y registers)
  (if (= (get-value x registers) 0)
      (get-value y registers)
      nil))

(defun jnz-reg (x y registers)
  (if (/= (get-value x registers) 0)
      (get-value y registers)
      nil))

(defun interpret-next (index instructions registers callback)
  (let ((next-index (+ index 1))
        (instruction (aref instructions index)))
    (case (car instruction)
      (set (set-reg (cadr instruction) (caddr instruction) registers))
      (sub (sub-reg (cadr instruction) (caddr instruction) registers))
      (mod (mod-reg (cadr instruction) (caddr instruction) registers))
      (mul (progn (funcall callback)
                  (mul-reg (cadr instruction) (caddr instruction) registers)))
      (jnz (let ((jmp (jnz-reg (cadr instruction) (caddr instruction) registers)))
             (if jmp
                 (setf next-index (+ index jmp)))))
      (jez (let ((jmp (jez-reg (cadr instruction) (caddr instruction) registers)))
             (if jmp
                 (setf next-index (+ index jmp)))))
      (comment nil)
      (otherwise (error (format nil "unknown instruction ~a" instruction))))
    next-index))

(defun terminated (index instructions)
  (or (< index 0)
      (>= index (length instructions))))

(defun part-1-solver (instructions)
  (let ((index 0)
        (mul-count 0)
        (registers (make-hash-table)))
    (loop while (not (terminated index instructions))
       do (setf index (interpret-next index instructions registers 
                          (lambda () (incf mul-count)))))
    mul-count))

(defun part-2-solver (instructions)
  (let ((index 0)
        (registers (make-hash-table)))
    (setf (gethash 'a registers) 1)
    (loop while (not (terminated index instructions))
       do (setf index (interpret-next index instructions registers (lambda ()))))
    (gethash 'h registers)))

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

(defun optimized-instructions ()
  (collect-instructions "coprocessor-optimized"))

(defun real-instructions ()
  (collect-instructions "coprocessor-input.txt"))

