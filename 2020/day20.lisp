(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :trivia)
  (ql:quickload :trivia.ppcre)
  (ql:quickload :cl-ppcre)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind)
  (ql:quickload :alexandria)
  (ql:quickload :cl-arrows)
  (load "../2018/queue.lisp")
  (load "../2018/priority-queue.lisp"))

(defpackage :day20
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day20)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input20" using #'read-line)
        (collect line)))

(defun read-tile (tile-id lines)
  (bind (((:values tile rest)
          (iter (for rem first lines then (cdr rem))
                (while (> (length (car rem)) 0))
                (collect (car rem) into tile result-type 'vector)
                (finally (return (values tile (cdr rem))))))
         ((:values top bottom left right)
          (iter (with top-mask = 0)
                (with bottom-mask = 0)
                (with left-mask = 0)
                (with right-mask = 0)
                (for i from (- (length (aref tile 0)) 1) downto 0) 
                (for j from 0)
                (for tch in-string (aref tile 0))
                (for bch in-string (aref tile (- (length tile) 1)))
                (when (char= tch #\#)
                  (setf top-mask (logior top-mask (ash 1 i))))
                (when (char= bch #\#)
                  (setf bottom-mask (logior bottom-mask (ash 1 i))))
                (when (char= (aref (aref tile j) 0) #\#)
                  (setf left-mask (logior left-mask (ash 1 i))))
                (when (char= (aref (aref tile j) (- (length (aref tile 0)) 1)) #\#)
                  (setf right-mask (logior right-mask (ash 1 i))))
                (finally (return (values top-mask bottom-mask left-mask right-mask))))))
    (values (vector top right bottom left tile-id) rest)))

(defun read-tiles (lines)
  (iter (for rem first lines then (cdr rem))
        (while rem)
        (match (car rem)
          ((ppcre "Tile (\\d+):" (read tile-id)) 
           (bind (((:values tile rest) (read-tile tile-id (cdr rem))))
             (setf rem rest)
             (collect tile))))))

(defun flip-side (side) 
  (iter (with result = 0)
        (for i from 0 to 9)
        (setf result (logior result (logand (ash 1 (- 9 i)) side)))
        (finally (return result))))

(defun matching-side (side-number)
  (mod (+ side-number 2) 4))

(defun required-rotation (one-side-num other-side-num)
  (let ((required-side (matching-side one-side-num)))
    (mod (+ (- required-side other-side-num) 4) 4)))

(defun rotate-tile (tile rotation)
  (match tile
    ((vector* top right bottom left id)
     (case rotation
       (0 tile)
       (1 (vector left top right bottom id))
       (2 (vector bottom left top right id))
       (3 (vector right bottom left top))))))

(defun flip-tile (tile side-number)
  (match tile
    ((vector* top right bottom left id)
     (case (mod side-number 2) 
       (0 (vector (flip-side top) left (flip-side bottom) right id))
       (1 (vector bottom (flip-side right) top (flip-side left) id))))))

(defun flip-rotate-tile (tile rotation side-num)
  (rotate-tile (flip-tile tile (mod (+ side-num 1) 4)) (mod (+ rotation 2) 4)))

(defun transform (tile one-side-num other-side-num flipped callback)
  (let ((rotation (required-rotation one-side-num other-side-num)))
    (if flipped
        (progn 
          (funcall callback (rotate-tile (flip-tile other-side-num tile) rotation))
          (funcall callback (flip-rotate-tile (flip-tile other-side-num tile) 
                                              rotation other-side-num)))
        (progn 
          (funcall callback (rotate-tile tile rotation))
          (funcall callback (flip-rotate-tile tile rotation other-side-num))))))

(defun fits-up (one other)
  (= (aref one 0) (aref other 2)))

(defun fits-down (one other)
  (= (aref one 2) (aref other 0)))

(defun fits-left (one other)
  (= (aref one 3) (aref other 1)))

(defun fits-right (one other)
  (= (aref one 1) (aref other 3)))

(defun fits-with-neighbours (tile grid coord)
  (let ((r (car coord))
        (c (cdr coord)))
    (and (aif (gethash (cons (- r 1) c) grid) 
              (fits-up tile it)
              t)
         (aif (gethash (cons (+ r 1) c) grid)
              (fits-down tile it)
              t)
         (aif (gethash (cons r (- c 1)) grid)
              (fits-left tile it)
              t)
         (aif (gethash (cons r (+ c 1)) grid)
              (fits-right tile it)
              t))))

(defun matching-orientations (side-number side other callback)
  (iter (for i from 0 to 3)
        (when (= (aref other i) side)
          (transform other side-number i nil callback))
        (when (= (flip-side (aref other i)) side)
          (transform other side-number i t callback))))

(defun fitting-matches (side-number side other coord grid callback)
  (bind (((r c) coord))
    (matching-orientations side-number side other
                           (lambda (m) 
                             (case side-number
                               (0 (when (fits-with-neighbours m grid (cons (- r 1) c))
                                    (funcall callback m)))
                               (1 (when (fits-with-neighbours m grid (cons r (+ c 1)))
                                    (funcall callback m)))
                               (2 (when (fits-with-neighbours m grid (cons (+ r 1) c))
                                    (funcall callback m)))
                               (3 (when (fits-with-neighbours m grid (cons r (+ c 1)))
                                    (funcall callback m))))))))


(defun find-matching-tiles (tiles)
  (let ((grid (make-hash-table :test 'equal))
        (used (make-hash-table :test 'equal))
        solved)
    (labels ((rec (tile coord)
               (if (= (hash-table-count grid) (length tiles))
                   grid
                   (bind (((r c) coord))
                     (when (not (gethash (cons (+ r 1) c) grid))
                       (iter (for other in tiles)
                             (when (not (gethash (aref other 4) used))
                               (fitting-matches 0 (aref tile 0) other coord grid
                                                (lambda (m) ()))))))))))))
