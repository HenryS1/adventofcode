(load "../2018/queue.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day17
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day17)

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
  (with-open-file (f "input17")
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
                           (enqueue out output)
                           next-i)))
              (5 (setf i (jit i codes base)))
              (6 (setf i (jif i codes base)))
              (7 (setf i (lt i codes base)))
              (8 (setf i (equals i codes base)))
              (9 (setf i (set-base i codes))))
            (finally (return nil)))))

(defun collect-output (computer)
  (let ((str (map 'string #'code-char (car (output computer))))
        (*read-eval*))
    (with-input-from-string (s str)
      (iter (for line in-stream s using #'read-line)
            (when (search "." line)
              (collect line))))))

(defun draw-map ()
  (let* ((computer (make-computer (read-program))))
    (compute computer)
    (collect-output computer)))

(defun find-intersections (mp)
  (iter outer
        (for row in mp)
        (for row-p previous row)
        (for row-pp previous row-p)
        (for r from 0)
        (iter (for c from 1 to (- (length row) 2))
              (when (and row-p row-pp
                         (char= #\# (aref row-p c) (aref row-pp c) (aref row c)
                                (aref row-p (- c 1)) (aref row-p (+ c 1))))
                (in outer (sum (* (- r 1) c)))))))

(defun read-test ()
  (iter (for line in-file "inputt" using #'read-line)
        (collect line)))

(defun next-coord (coord orientation)
;  (format t "ORIENTATION ~a~%" orientation)
  (case orientation 
    (UP (cons (- (car coord) 1) (cdr coord)))
    (DOWN (cons (+ (car coord) 1) (cdr coord)))
    (RIGHT (cons (car coord) (+ (cdr coord) 1)))
    (LEFT (cons (car coord) (- (cdr coord) 1)))))

(defun in-bounds (coord mp)
  (and (>= (car coord) 0)
       (>= (cdr coord) 0)
       (< (car coord) (length mp))
       (< (cdr coord) (length (aref mp 0)))))

(defun find-ahead (mp orientation coord)
  (let ((next (next-coord coord orientation)))
;    (format t "NEXT COORD ~a~%" next)
    (and (in-bounds next mp) (char= (aref (aref mp (car next)) (cdr next)) #\#) 
         next)))

(defun turn-right (orientation)
  (case orientation
    (UP 'RIGHT)
    (RIGHT 'DOWN)
    (DOWN 'LEFT)
    (LEFT 'UP)))

(defun turn-left (orientation)
  (case orientation
    (UP 'LEFT)
    (LEFT 'DOWN)
    (DOWN 'RIGHT)
    (RIGHT 'UP)))

(defun find-next-square (mp orientation coord)
  (acond ((find-ahead mp orientation coord) (list orientation it nil))
         ((find-ahead mp (turn-right orientation) coord) (list (turn-right orientation) it 'R))
         ((find-ahead mp (turn-left orientation) coord) (list (turn-left orientation) it 'L))))

(defun find-orientation (sq)
  (case sq
    (#\> 'RIGHT)
    (#\< 'LEFT)
    (#\v 'DOWN)
    (#\^ 'UP)))

(defun find-start (mp)
  (iter outer
        (for r from 0 to (- (length mp) 1))
        (iter (for c from 0 to (- (length (aref mp 0)) 1))
              (for sq = (aref (aref mp r) c))
              (when (find sq '(#\> #\< #\^ #\v))
                (return-from outer (cons (find-orientation sq) (cons r c)))))))

(defun find-path (mp)
  (iter (with (orientation . coord) = (find-start mp))
        (with current-moves = 0)
        (for next = (find-next-square mp orientation coord))
        (while (or next (> current-moves 0)))
        (when next 
          (for (next-orientation next-c turn) = next)
          (setf orientation next-orientation
                coord next-c)
          (if turn 
              (progn (when (> current-moves 0) (collect current-moves))
                     (collect turn)
                     (setf current-moves 1))
              (incf current-moves)))
        (when (not next)
          (collect current-moves)
          (setf current-moves 0))))

(defun answer-1 ()
  (find-intersections (draw-map)))

(defun show-path ()
  (find-path (map 'vector #'identity (draw-map))))

(defun answer-2 ()
  (let ((computer (make-computer (let ((cs (read-program))) 
                                   (setf (aref cs 0) 2)
                                   cs)))
        (instructions (list "A,A,B,C,C,A,C,B,C,B"
                             "L,4,L,4,L,6,R,10,L,6"
                             "L,12,L,6,R,10,L,6"
                             "R,8,R,10,L,6")))
    (iter (for instruction in instructions)
          (iter (for c in-string instruction)
                (enqueue-inp (char-code c) computer))
          (enqueue-inp (char-code #\newline) computer))
    (enqueue-inp (char-code #\n) computer)
    (enqueue-inp (char-code #\newline) computer)
    (compute computer)
    (car (last (car (output computer))))))
