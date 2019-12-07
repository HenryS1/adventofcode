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

(defun find-termination (in1 in2 codes)
  (parse-integer (with-output-to-string (s) 
       (let ((*standard-output* s))
          (iter (with i = 0)
                (with inp = in1)
                (for code = (aref codes i))
                (case (mod code 100)
                  (99 (leave))
                  (1 (setf i (plus i codes)))
                  (2 (setf i (mult i codes)))
                  (3 (progn (setf i (save inp i codes))
                            (setf inp in2)))
                  (4 (setf i (output i codes)))
                  (5 (setf i (jit i codes)))
                  (6 (setf i (jif i codes)))
                  (7 (setf i (lt i codes)))
                  (8 (setf i (equals i codes))))))
       s)))

(defun read-program ()
  (with-open-file (f "input7")
    (map 'vector #'identity (ints (read-line f)))))

(defun find-max-signal (codes)
  (iter outer (for prog = (copy-seq codes))
        (for p1 from 0 to 4)
        (for o1 = (find-termination p1 0 prog))
        (iter (for p2 from 0 to 4)
              (when (= p1 p2)
                (next-iteration))
              (setf prog (copy-seq codes))
              (for o2 = (find-termination p2 o1 prog))
              (iter (for p3 from 0 to 4)
                    (when (or (= p3 p2) (= p3 p1))
                      (next-iteration))
                    (setf prog (copy-seq codes))
                    (for o3 = (find-termination p3 o2 prog))
                    (iter (for p4 from 0 to 4)
                          (when (or (= p4 p3) (= p4 p2) (= p4 p1))
                            (next-iteration))
                          (setf prog (copy-seq codes))
                          (for o4 = (find-termination p4 o3 prog))
                          (iter (for p5 from 0 to 4)
                                (when (or (= p5 p4) (= p5 p3) (= p5 p2) (= p5 p1))
                                  (next-iteration))
                                (setf prog (copy-seq codes))
                                (for o5 = (find-termination p5 o4 prog))
                                (in outer (maximize o5))))))))

(defun answer-1 ()
  (find-max-signal (read-program)))
