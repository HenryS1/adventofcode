(load "../2018/queue.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day21
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day21)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun eval-value (mode v codes base)
  (case mode
    (0 (aref codes v))
    (1 v)
    (2 (aref codes (+ v base)))))

(defun digit (n i)
  (mod (floor n (expt 10 i)) 10))

(defmacro with-params (ind codes base &rest body)
  (with-gensyms (encoding m1 m2 m3)
    `(let* ((,encoding (aref ,codes ,ind))
            (,m1 (digit ,encoding 2))
            (,m2 (digit ,encoding 3))
            (,m3 (digit ,encoding 4))
            (v1 (eval-value ,m1 (aref ,codes (+ ,ind 1)) ,codes ,base))
            (v2 (eval-value ,m2 (aref ,codes (+ ,ind 2)) ,codes ,base))
            (dest (if (< (+ ,ind 3) (length ,codes))
                      (case ,m3
                        (0 (aref ,codes (+ ,ind 3)))
                        (2 (+ ,base (aref ,codes (+ ,ind 3))))
                        (t (error "this shouldn't happen")))
                      0)))
       (declare (ignorable dest))
       ,@body)))

(defun binop (ind codes op base)
  (with-params ind codes base
     (setf (aref codes dest)
           (funcall op v1 v2))
     (+ ind 4)))

(defun mult (ind codes base)
  (binop ind codes #'* base))

(defun plus (ind codes base)
  (binop ind codes #'+ base))

(defun save (inp ind codes base)
  (let* ((encoding (aref codes ind))
         (mode (digit encoding 2))
         (offset (if (= mode 2) base 0)))
    (setf (aref codes (+ offset (aref codes (+ ind 1)))) inp)
         (+ ind 2)))

(defun jit (ind codes base)
  (with-params ind codes base
               (if (/= v1 0) v2 (+ ind 3))))

(defun jif (ind codes base)
  (with-params ind codes base
               (if (= v1 0) v2 (+ ind 3))))

(defun lt (ind codes base)
  (with-params ind codes base
               (if (< v1 v2) (setf (aref codes dest) 1) (setf (aref codes dest) 0))
               (+ ind 4)))

(defun equals (ind codes base)
  (with-params ind codes base
               (if (= v1 v2) (setf (aref codes dest) 1) (setf (aref codes dest) 0))
               (+ ind 4)))

(defun read-program ()
  (with-open-file (f "input21")
    (let ((program (map 'vector #'identity (ints (read-line f)))))
      (concatenate 'vector program (make-array (* 2 (length program)) :initial-element 0)))))

(defun output-for-next (ind codes base)
  (let* ((encoding (aref codes ind))
         (mode (digit encoding 2)))
    (list (eval-value mode (aref codes (+ ind 1)) codes base)
          (+ ind 2))))

(defmacro set-base (ind codes)
  (with-gensyms (encoding mode)
    `(let* ((,encoding (aref ,codes ,ind))
            (,mode (digit ,encoding 2)))
       (incf base (eval-value ,mode (aref ,codes (+ ,ind 1)) ,codes base))
       (+ ,ind 2))))

(defclass computer ()
  ((i :accessor i :initarg :i :initform 0)
   (codes :accessor codes :initarg :codes :type 'vector)
   (base :accessor base :initarg :base :initform 0)
   (input :accessor input :initform (make-queue))
   (output :accessor output :initform (make-queue))))

(defun copy-computer (computer)
  (make-instance 'computer :i (i computer) :base (base computer) 
                 :codes (copy-seq (codes computer))))

(defun make-computer (codes)
  (make-instance 'computer :codes (copy-seq codes)))

(defun enqueue-inp (input computer)
  (enqueue input (input computer)))

(defun poll-out (computer)
  (poll (output computer)))

(defmethod compute (computer)
  (with-accessors ((base base) (codes codes) (i i) (input input) (output output))
      computer
      (iter (for code = (aref codes i))
            (case (mod code 100)
              (99 (leave t))
              (1 (setf i (plus i codes base)))
              (2 (setf i (mult i codes base)))
              (3 (if (non-empty input)
                     (let ((inp (poll input)))
                       (setf i (save inp i codes base)))
                     (finish)))
              (4 (setf i (destructuring-bind (out next-i) 
                             (output-for-next i codes base)
;                           (format t "OUTPUTTING ~a~%" out)
                           (enqueue out output)
                           next-i)))
              (5 (setf i (jit i codes base)))
              (6 (setf i (jif i codes base)))
              (7 (setf i (lt i codes base)))
              (8 (setf i (equals i codes base)))
              (9 (setf i (set-base i codes))))
            (finally (return nil)))))

(defun send-commands (&rest commands)
  (let ((computer (make-computer (read-program))))
    (iter (for command in commands)
          (iter (for c in-string command)
                (enqueue-inp (char-code c) computer))
          (enqueue-inp (char-code #\newline) computer))
    (compute computer)
    (car (output computer))))

(defun render-ascii (output)
  (map 'string #'code-char output))

;; obstacles
;; #####.###########
;; #####...#########
;; #####..#.########
;; #####.#..########
;; #####.#.##...####

(defun answer-1 ()
  (car (last (send-commands "NOT A T" "NOT B J" "OR T J"
                            "NOT C T" "OR T J" "AND D J"  "WALK"))))

(defun answer-2 ()
  (car (las (send-commands "NOT A T" "NOT B J" "OR T J" "NOT C T" "OR T J"
                           "AND D J" "OR J T" "AND E T" "OR H T" "AND T J"  "RUN"))))
