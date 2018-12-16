(load "utilities.lisp")
(ql:quickload :cl-ppcre)

(defparameter *input-filename* "chronal-classification-input.txt")

(defun parse-register-state (line)
  (multiple-value-bind (m rs) 
      (cl-ppcre:scan-to-strings ".*\\[(\\d+),\\s+(\\d+),\\s+(\\d+),\\s+(\\d+)\\].*" line)
    (declare (ignore m))
    (map 'vector #'parse-integer rs)))

(defun parse-operation (line)
  (map 'vector #'parse-integer (cl-ppcre:split "\\s+" line)))

(defun parse-constraint (lines)
  (let ((before (parse-register-state (car lines)))
        (operation (parse-operation (cadr lines)))
        (after (parse-register-state (caddr lines))))
    (list before operation after)))

(defun collect-constraints (lines)
  (loop for rest = lines then (cdr rest)
       while rest
       with program = nil
       with constraints = nil
       when (search "Before" (car rest))
       do (multiple-value-bind (taken remaining) (take-drop 3 rest)
            (setf rest remaining)
            (push (parse-constraint taken) constraints))
       when (and (> (length (car rest)) 0)
                 (digit-char-p (aref (car rest) 0)))
       do (push (map 'vector #'parse-integer (cl-ppcre:split "\\s+" (car rest))) program)
       finally (return (values constraints (reverse program)))))

(defun read-constraints (filename)
  (collect-constraints (read-lines filename)))

(defun is-immediate (op-name)
  (let ((name-str (symbol-name op-name)))
    (char= (aref name-str (1- (length name-str))) #\i)))

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

(defparameter *register-funs* 
  (list #'adddi #'adddr #'muli #'mulr #'bani #'banr #'bori #'borr
        #'seti #'setr #'gtir #'gtri #'gtrr #'eqir #'eqri #'eqrr))

(defun satisfies-constraint (register-fun constraint)
  (destructuring-bind (before args after) (mapcar #'copy-seq constraint)
    (let ((a (aref args 1)) (b (aref args 2)) (c (aref args 3)))
      (funcall register-fun a b c before)
      (every #'= before after))))

(defun count-possible-opcodes (constraint)
  (count-if (lambda (register-fun) (satisfies-constraint register-fun constraint)) 
            *register-funs*))

(defun solution-part-1 ()
  (count-if (lambda (n) (>= n 3)) 
            (mapcar #'count-possible-opcodes (read-constraints *input-filename*))))

(defun possible-opcodes (constraint)
  (remove-if-not (lambda (register-fun) (satisfies-constraint register-fun constraint))
                 *register-funs*))

(defun identifier (constraint)
  (aref (cadr constraint) 0))

(defun solve-constraints (constraints)
  (let ((assignments (make-array 16 :initial-element nil))
        (assigned (make-hash-table)))
    (labels ((recur (remaining)
               (if (not (null remaining))
                   (let* ((constraint (car remaining))
                          (id (identifier constraint))
                          (register-fun (aref assignments id)))
                     (if register-fun
                         (when (satisfies-constraint register-fun constraint)
                             (recur (cdr remaining)))
                         (loop for fun in (possible-opcodes constraint)
                            when (not (gethash fun assigned))
                            do (setf (aref assignments id) fun) 
                              (setf (gethash fun assigned) t)
                              (recur (cdr constraints))
                              (remhash fun assigned)
                              (setf (aref assignments id) nil)
                            finally (return nil))))
                   (return-from solve-constraints assignments))))
      (recur constraints))))

(defun interpret-program (operator-assignments program)
  (let ((registers (vector 0 0 0 0)))
    (loop for operation in program 
       do (let ((register-fun (aref operator-assignments (aref operation 0))))
            (funcall register-fun (aref operation 1) 
                     (aref operation 2)
                     (aref operation 3)
                     registers)))
    registers))

(defun solution-part-2 ()
  (multiple-value-bind (constraints program) (read-constraints *input-filename*)
    (let ((operator-assignments (solve-constraints constraints)))
      (interpret-program operator-assignments program))))
