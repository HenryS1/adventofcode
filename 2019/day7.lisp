(load "../2018/queue.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload :metabang-bind)
(ql:quickload :iterate)
(ql:quickload :alexandria)
(ql:quickload :anaphora)

(defpackage :day7
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day7)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun eval-value (mode v codes)
  (case mode
    (0 (aref codes v))
    (1 v)))

(defun digit (n i)
  (mod (floor n (expt 10 i)) 10))

(defmacro with-params (ind codes &rest body)
  (with-gensyms (encoding m1 m2)
    `(let* ((,encoding (aref ,codes ,ind))
            (,m1 (digit ,encoding 2))
            (,m2 (digit ,encoding 3))
            (v1 (eval-value ,m1 (aref ,codes (+ ,ind 1)) ,codes))
            (v2 (eval-value ,m2 (aref ,codes (+ ,ind 2)) ,codes)))
       ,@body)))

(defun binop (ind codes op)
  (with-params ind codes
     (setf (aref codes (aref codes (+ ind 3)))
           (funcall op v1 v2))
      (+ ind 4)))

(defun mult (ind codes)
  (binop ind codes #'*))

(defun plus (ind codes)
  (binop ind codes #'+))

(defun save (inp ind codes)
  (progn (setf (aref codes (aref codes (+ ind 1))) inp)
         (+ ind 2)))

(defun output (ind codes)
  (let* ((encoding (aref codes ind))
         (mode (digit encoding 2)))
    (format t "~a~%" (eval-value mode (aref codes (+ ind 1)) codes))
    (+ ind 2)))

(defun jit (ind codes)
  (with-params ind codes
               (if (/= v1 0) v2 (+ ind 3))))

(defun jif (ind codes)
  (with-params ind codes
               (if (= v1 0) v2 (+ ind 3))))

(defun lt (ind codes)
  (with-params ind codes
   (let ((dest (aref codes (+ ind 3))))
     (if (< v1 v2) (setf (aref codes dest) 1) (setf (aref codes dest) 0))
     (+ ind 4))))

(defun equals (ind codes)
  (with-params ind codes
    (let ((dest (aref codes (+ ind 3))))
     (if (= v1 v2) (setf (aref codes dest) 1) (setf (aref codes dest) 0))
     (+ ind 4))))

(defun read-program ()
  (with-open-file (f "input7")
    (map 'vector #'identity (ints (read-line f)))))

(defun output-for-next (ind codes)
  (let* ((encoding (aref codes ind))
         (mode (digit encoding 2)))
    (list (eval-value mode (aref codes (+ ind 1)) codes)
          (+ ind 2))))

(defun make-amp (input-buffer output-buffer codes &optional (final-output nil))
  (let ((i 0))
    (lambda () 
      (iter (for code = (aref codes i))
            (case (mod code 100)
              (99 (leave t))
              (1 (setf i (plus i codes)))
              (2 (setf i (mult i codes)))
              (3 (if (non-empty input-buffer)
                     (setf i (save (poll input-buffer) i codes))
                     (finish)))
              (4 (setf i (destructuring-bind (output next-i) 
                                 (output-for-next i codes)
                               (when final-output
                                 (setf (aref final-output 0) output))
                               (enqueue output output-buffer)
                               next-i)))
              (5 (setf i (jit i codes)))
              (6 (setf i (jif i codes)))
              (7 (setf i (lt i codes)))
              (8 (setf i (equals i codes))))
            (finally (return nil))))))

(defun run-feedback-loop (buf1 buf2 buf3 buf4 buf5 codes)
  (iter (with thruster = (vector 0))
        (with amp1 = (make-amp buf1 buf2 (copy-seq codes)))
        (with amp2 = (make-amp buf2 buf3 (copy-seq codes)))
        (with amp3 = (make-amp buf3 buf4 (copy-seq codes)))
        (with amp4 = (make-amp buf4 buf5 (copy-seq codes)))
        (with amp5 = (make-amp buf5 buf1 (copy-seq codes) thruster))
        (for finished = (list (funcall amp1) 
                              (funcall amp2)
                              (funcall amp3)
                              (funcall amp4)
                              (funcall amp5)))
        (until (every #'identity finished))
        (finally (return (aref thruster 0)))))

(defun find-max-feedback (codes iter-range)
  (iter outer 
        (for p1 in iter-range)
        (iter (for p2 in iter-range)
              (when (= p1 p2)
                (next-iteration))
              (iter (for p3 in iter-range)
                    (when (or (= p3 p2) (= p3 p1))
                      (next-iteration))
                    (iter (for p4 in iter-range)
                          (when (or (= p4 p3) (= p4 p2) (= p4 p1))
                            (next-iteration))
                          (iter (for p5 in iter-range)
                                (when (or (= p5 p4) (= p5 p3) (= p5 p2) (= p5 p1))
                                  (next-iteration))
                                (for buf1 = (make-queue p1 0))
                                (for buf2 = (make-queue p2))
                                (for buf3 = (make-queue p3))
                                (for buf4 = (make-queue p4))
                                (for buf5 = (make-queue p5))
                                (in outer (maximize 
                                           (run-feedback-loop buf1 buf2 buf3 
                                                              buf4 buf5 codes)))))))))

(defun answer-1 ()
  (find-max-feedback (read-program) '(0 1 2 3 4)))

(defun answer-2 ()
  (find-max-feedback (read-program) '(5 6 7 8 9)))
