(load "../2018/queue.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day19
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day19)

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
  (with-open-file (f "input19")
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

(defun tractor-beam-value (computer x y)
  (enqueue-inp x computer)
  (enqueue-inp y computer)
  (compute computer)
  (poll-out computer))

(defun affected-points ()
  (iter outer
        (with program = (read-program))
        (for x from 0 to 49)
        (iter (for y from 0 to 49)
              (for computer = (make-computer program))
              (enqueue-inp x computer)
              (enqueue-inp y computer)
              (compute computer)
              (in outer (sum (poll-out computer))))))

(defun answer-1 () (affected-points))

(defun affected-points-map ()
  (iter outer
        (with program = (read-program))
        (for x from 0 to 49)
        (collect (iter (for y from 0 to 49)
                       (for computer = (make-computer program))
                       (enqueue-inp x computer)
                       (enqueue-inp y computer)
                       (compute computer)
                       (collect (poll-out computer))))))

(defun compute-coord (program coord)
  (let ((computer (make-computer program)))
    (enqueue-inp (car coord) computer)
    (enqueue-inp (cdr coord) computer)
    (compute computer)
    (poll-out computer)))

(defun down (coord)
  (cons (+ (car coord) 1) (cdr coord)))

(defun right (coord)
  (cons (car coord) (+ (cdr coord) 1)))

(defun next-top (program top)
  (iter (for nxt first (right top) then (down nxt))
        (for nxt-p previous nxt)
        (while (= (compute-coord program nxt) 0))
        (finally (return nxt))))

(defun next-bottom (program bottom)
  (iter (for nxt first (right bottom) then (down nxt))
        (for nxt-p previous nxt)
        (while (= (compute-coord program nxt) 1))
        (finally (return nxt-p))))

(defun find-edges ()
  (iter (with tops = (make-hash-table :test 'equal))
        (with bottoms = (make-hash-table :test 'equal))
        (with program = (read-program))
        (for top first '(8 . 15) then (next-top program top))
        (for bottom first '(9 . 15) then (next-bottom program bottom))
        (while (< (- (car bottom) (car top)) 400))
        (setf (gethash (cdr top) tops) (car top))
        (setf (gethash (cdr bottom) bottoms) (car bottom))
        (finally (return (list bottoms tops)))))

(defun find-first-square ()
  (bind (((bottoms tops) (find-edges)))
    (iter (for x from 50 to 2000)
          (for top-left = (gethash x tops))
          (for top-right = (gethash (+ x 99) tops))
          (for bottom-left = (gethash x bottoms))
          (for bottom-right = (gethash (+ x 99) bottoms))
          (when (and top-left top-right bottom-left bottom-right
                     (>= (- bottom-left top-right) 99))
            (return (+ x (* top-right 10000)))))))
