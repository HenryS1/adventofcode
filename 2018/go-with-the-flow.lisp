(load "utilities.lisp")

(defun replace-add (instruction)
  (case (car instruction) 
    (addr (cons 'adddr (cdr instruction)))
    (addi (cons 'adddi (cdr instruction)))
    (otherwise instruction)))

(defun parse-instruction (line)
  (let (*read-eval*)
    (with-input-from-string (s (remove-if (lambda (c) (char= c #\#)) line))
      (replace-add (loop for token = (read s nil nil)
                      while token collect token)))))

(defparameter *input-file* "go-with-the-flow-input.txt")
(defparameter *test-input-file* "go-with-the-flow-test-input.txt")

(defun read-instructions (filename)
  (let ((all (read-lines filename #'parse-instruction)))
    (cons (cadar all) (coerce (cdr all) 'vector))))

(defmacro defregfun (name op)
  (let ((i-name (append-to-symbol name "I"))
        (r-name (append-to-symbol name "R")))
    `(progn 
       (defun ,i-name (a b c registers)
         (setf (aref registers c)
               (,op (aref registers a) b)))
       (defun ,r-name (a b c registers)
         (setf (aref registers c)
               (,op (aref registers a) (aref registers b)))))))

(defregfun addd +)
(defregfun mul *)
(defregfun ban logand)
(defregfun bor logior)

(defun setr (a b c registers)
  (declare (ignore b))
  (setf (aref registers c) (aref registers a)))

(defun seti (a b c registers)
  (declare (ignore b))
  (setf (aref registers c) a))

(defun gtir (a b c registers)
  (setf (aref registers c)
        (if (> a (aref registers b)) 1 0)))

(defregfun gtr (lambda (a b) (if (> a b) 1 0)))

(defun eqir (a b c registers)
  (setf (aref registers c)
        (if (= a (aref registers b)) 1 0)))

(defregfun eqr (lambda (a b) (if (= a b) 1 0)))

(defun ip (i registers) 
  (setf (aref registers 6) i))

(defun interpret-instruction (instruction registers)
  (destructuring-bind (name a b c) instruction
    (funcall name a b c registers)))

(defun get-next-instruction-pointer (registers)
  (1+ (aref registers (aref registers 6))))

(defun step-program (instructions registers instruction-pointer)
  (interpret-instruction (aref instructions instruction-pointer) registers))

(defun set-instruction-pointer-in-register (instruction-pointer registers)
  (setf (aref registers (aref registers 6)) instruction-pointer))

(defun run-to-end (instructions registers)
  (loop for instruction-pointer = 0 then (get-next-instruction-pointer registers)
     while (<= 0 instruction-pointer (1- (length instructions)))
     do (set-instruction-pointer-in-register instruction-pointer registers)
       (step-program instructions registers instruction-pointer)))

(defun initialise-and-run (instruction-pointer-register instructions registers)
  (setf (aref registers 6) instruction-pointer-register)
  (run-to-end instructions registers))

(defun test-1 ()
  (destructuring-bind (instruction-pointer-register . instructions)
      (read-instructions *test-input-file*)
    (let ((registers (vector 0 0 0 0 0 0 0)))
      (initialise-and-run instruction-pointer-register instructions registers)
      registers)))

(defun solution-part-1 ()
  (destructuring-bind (instruction-pointer-register . instructions) 
      (read-instructions *input-file*)
    (let ((registers (vector 0 0 0 0 0 0 0)))
      (initialise-and-run instruction-pointer-register instructions registers)
      registers)))

(defun solution-part-2 ()
  "The program is the divisor sum of 10551319"
  (loop for i from 1 to 10551319 when (= (mod 10551319 i) 0) sum i))
