(load "../2018/queue.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day13
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day13)

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
  (with-open-file (f "input13")
    (let ((program (map 'vector #'identity (ints (read-line f)))))
      (concatenate 'vector program (make-array (* 10 (length program)) :initial-element 0)))))

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

(defun make-computer (input-buffer output-buffer codes &optional (final-output nil))
  (let ((i 0)
        (base 0))
    (lambda () 
      (iter (for code = (aref codes i))
            (case (mod code 100)
              (99 (leave t))
              (1 (setf i (plus i codes base)))
              (2 (setf i (mult i codes base)))
              (3 (if (non-empty input-buffer)
                     (let ((inp (poll input-buffer)))
                       (setf i (save inp i codes base)))
                     (finish)))
              (4 (setf i (destructuring-bind (output next-i) 
                                 (output-for-next i codes base)
                           (when final-output
                             (setf (aref final-output 0) output))
                           (enqueue output output-buffer)
                           next-i)))
              (5 (setf i (jit i codes base)))
              (6 (setf i (jif i codes base)))
              (7 (setf i (lt i codes base)))
              (8 (setf i (equals i codes base)))
              (9 (setf i (set-base i codes))))
            (finally (return nil))))))

(defun move (square turn d)
  (cond ((and (equal d 'N) (= turn 0))
         (list (cons (car square) (- (cdr square) 1)) 'W))
        ((and (equal d 'N) (= turn 1))
         (list (cons (car square) (+ (cdr square) 1)) 'E))
        ((and (equal d 'E) (= turn 0))
         (list (cons (- (car square) 1) (cdr square)) 'N))
        ((and (equal d 'E) (= turn 1))
         (list (cons (+ (car square) 1) (cdr square)) 'S))
        ((and (equal d 'S) (= turn 0)) 
         (list (cons (car square) (+ (cdr square) 1)) 'E))
        ((and (equal d 'S) (= turn 1))
         (list (cons (car square) (- (cdr square) 1)) 'W))
        ((and (equal d 'W) (= turn 0))
         (list (cons (+ (car square) 1) (cdr square)) 'S))
        ((and (equal d 'W) (= turn 1))
         (list (cons (- (car square) 1) (cdr square)) 'N))))

(defun collect-output (output-buffer)
  (iter (with board = (make-hash-table :test 'equal))
        (while (non-empty output-buffer))
        (for x = (poll output-buffer))
        (for y = (poll output-buffer))
        (for tile = (poll output-buffer))
        (count (= tile 2) into cnt)
        (if (>= x 0)
            (setf (gethash (cons x y) board) tile))
        (finally (return (cons cnt board)))))

(defun draw-tiles ()
  (let* ((input-buffer (make-queue))
         (output-buffer (make-queue))
         (codes (read-program))
         (comp (make-computer input-buffer output-buffer codes)))
    (iter (until (funcall comp)))
    (collect-output output-buffer)))

(defun convert-to-grid (table)
  (iter (with (mxx . mxy) = (bounds table))
        (with grid = (make-array (list (+ mxy 1) (+ mxx 1)) :initial-element 0))
        (for ((x . y) b) in-hashtable table)
        (setf (aref grid y x) b)
        (finally (return grid))))

(defun bounds (tiles)
  (iter (for ((x . y) b) in-hashtable tiles)
        (maximize x into mxx)
        (maximize y into mxy)
        (finally (return (cons mxx mxy)))))

(defun find-item (output-buffer item)
  (iter (for items first (car output-buffer) then (nthcdr 3 items))
        (while items)
        (for x = (first items))
        (for y = (second items))
        (for tl = (third items))
        (when (= tl item)
          (return (cons x y)))))

(defun find-next-input (ball paddle)
  (cond ((> (car ball) (car paddle)) 1)
          ((< (car ball) (car paddle)) -1)
          (t 0)))

(defun play-game ()
  (let* ((input-buffer (make-queue))
         (output-buffer (make-queue))
         (codes (let ((cs (read-program))) (setf (aref cs 0) 2) cs))
         (comp (make-computer input-buffer output-buffer codes))
         (grid (progn (funcall comp) 
                      (convert-to-grid (cdr (collect-output (copy-q output-buffer))))))
         (ball (find-item output-buffer 4))
         (paddle (find-item output-buffer 3)))
    (labels ((process-output (&optional (print-score nil))
               (iter (while (non-empty output-buffer))
                     (for x = (poll output-buffer))
                     (for y = (poll output-buffer))
                     (for tl = (poll output-buffer))
                     (cond ((= x -1) (when print-score (format t "SCORE ~a~%" tl)))
                           ((= tl 3) (setf 
                                      (aref grid (cdr paddle) (car paddle)) 0
                                      (aref grid y x) tl
                                      paddle (cons x y)))
                           ((= tl 4) (setf
                                      (aref grid (cdr ball) (car ball)) 0
                                      (aref grid y x) tl
                                      ball (cons x y)))))))
      (iter (enqueue (find-next-input ball paddle) input-buffer)
        (while (not (funcall comp)))
        (process-output)
        (finally (return (process-output t)))))))

(defun answer-2 () (play-game))
