(ql:quickload :cl-ppcre)

(defun increment-char (c)
  (if (char= c #\z) #\a (code-char (+ 1 (char-code c)))))

(defun increment (pwd)
  (labels ((rec (ind)
             (let ((c (aref pwd ind)))
               (when (char= c #\z)
                 (if (= ind 0) 
                     "aaaaaaaa"
                     (rec (- ind 1))))
               (setf (aref pwd ind) (increment-char c)))))
    (rec (- (length pwd) 1))
    pwd))

(defun has-increasing-sequence (pwd)
  (loop for i from 2 to (- (length pwd) 1)
     for c = (aref pwd i)
     for cp = (aref pwd (- i 1))
     for cpp = (aref pwd (- i 2))
     when (and (= (char-code c) (+ 1 (char-code cp)))
               (= (char-code cp) (+ 1 (char-code cpp))))
     return t))

(defun has-two-pairs (pwd)
  (cl-ppcre:scan "(\\w)\\1.*(\\w)\\2" pwd))

(defun allowed (pwd)
  (and (every (lambda (c) (and (char/= c #\i) (char/= c #\l) (char/= c #\o))) pwd)
       (has-increasing-sequence pwd)
       (has-two-pairs pwd)))

(defun find-password (start)
  (loop for pwd = (increment start) then (increment pwd)
     until (allowed pwd)
     finally (return pwd)))

(defun read-input () (with-open-file (f "input11") (read-line f nil nil)))

(defun part-one ()
  (find-password (read-input))) 

(defun part-two ()
  (find-password (find-password (read-input))))
