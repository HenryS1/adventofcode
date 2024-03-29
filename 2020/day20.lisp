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
                (finally (return (values tile rem)))))
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
                  (setf bottom-mask (logior bottom-mask (ash 1 j))))
                (when (char= (aref (aref tile j) 0) #\#)
                  (setf left-mask (logior left-mask (ash 1 j))))
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
        (setf result (logior result (ash (ash (logand (ash 1 (- 9 i)) side) (- i 9)) i)))
        (finally (return result))))

(defun matching-side (side-number)
  (mod (+ side-number 2) 4))

(defun required-rotation (one-side-num other-side-num)
  (let ((required-side (matching-side one-side-num)))
    (mod (+ (- required-side other-side-num) 4) 4)))

(defun rotate-tile (tile rotation)
;  (format t "rotate tile ~a~%" tile)
  (match tile
    ((vector* top right bottom left id)
     (case rotation
       (0 tile)
       (1 (vector left top right bottom id))
       (2 (vector bottom left top right id))
       (3 (vector right bottom left top id))))))

(defun flip-tile (tile side-number)
  (assert (<= 0 (mod side-number 2) 1))
  (match tile
    ((vector* top right bottom left id)
     (case (mod side-number 2) 
       (0 (vector (flip-side top) left (flip-side bottom) right id))
       (1 (vector bottom (flip-side right) top (flip-side left) id))))))

(defun flip-rotate-tile (tile rotation side-num)
;  (format t "flip rotate tile ~a~%" tile)
  (rotate-tile (flip-tile tile (mod (+ side-num 1) 4)) (mod (+ rotation 2) 4)))

(defun transform (tile one-side-num other-side-num flipped callback)
  (let ((rotation (required-rotation one-side-num other-side-num)))
    (assert (<= 0 rotation 3))
    (if flipped
        (progn 
          (funcall callback (rotate-tile (flip-tile tile other-side-num) rotation))
          (funcall callback (flip-rotate-tile (flip-tile tile other-side-num) 
                                              rotation other-side-num)))
        (progn 
          (funcall callback (rotate-tile tile rotation))
          (funcall callback (flip-rotate-tile tile rotation other-side-num))))))

(defun fits-up (one other)
  (= (aref one 0) (flip-side (aref other 2))))

(defun fits-down (one other) (fits-up other one))

(defun fits-right (one other)
  (= (aref one 1) (flip-side (aref other 3))))

(defun fits-left (one other) (fits-right other one))

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

(declaim (optimize (debug 3) (speed 0)))

(defun matching-orientations (side-number side other callback)
  (format t "considering other ~a for side ~a side-number ~a flip-side ~a~%" other side side-number (flip-side side))
  (iter (for i from 0 to 3)
        (when (= (aref other i) (flip-side side))
          (format t "side-number ~a side ~a other ~a~%" side-number side other)
          (transform other side-number i nil callback))
        (when (= (aref other i) side)
          (transform other side-number i t callback))))

(defun fitting-matches (side-number tile other next-coord grid)
  (let (ms)
    (format t "inside fitting matches for side-number ~a~%" side-number)
    (matching-orientations side-number (aref tile side-number) other
                           (lambda (m) 
                             (format t "matched m ~a with tile ~a~%" m tile)
                             (assert (not (null m)))
                             (if (fits-with-neighbours m grid next-coord)
                                 (progn (format t "side fits ~a~%" m)
                                   (push m ms))
                                 (progn (format t "didn't fit next-coord ~a m ~a~%" next-coord m)
                                   (iter (for (k v) in-hashtable grid)
                                         (format t "k ~a v ~a~%" k v))))))
    (format t "done with matching orientations~%")
    ms))

(defun move (coord side-number)
  (bind (((r . c) coord))
    (case side-number
      (0 (cons (- r 1) c))
      (1 (cons r (+ c 1)))
      (2 (cons (+ r 1) c))
      (3 (cons r (- c 1))))))

(defun find-matching-tiles (tiles)
  (format t "find matching~%")
  (let ((grid (make-hash-table :test 'equal))
        (used (make-hash-table :test 'equal)))
    (labels ((rec (tile coord side-number)
               (format t "tiles ~a~%" tiles)
               (format t "tile ~a~%" tile)
               (format t "side number ~a used count ~a~%" side-number (hash-table-count used))
               (if (= (hash-table-count grid) (length tiles))
                   (return-from find-matching-tiles grid)
                   (when (< side-number 4)
                     (let ((next-coord (move coord side-number)))
                       (when (not (gethash next-coord grid))
                         (format t "coord ~a next-coord ~a~%" coord next-coord)
                         (iter (for diff from 0 to 4)
                               (iter (for other in tiles)
                                     (when (not (gethash (aref other 4) used))
                                       (format t "fitting matches for side-number ~a~%" (mod (+ side-number diff) 4))
                                       (for ms = (fitting-matches (mod (+ side-number diff) 4) tile
                                                                  other next-coord grid))
                                       (format t "ms ~a~%" ms)
                                       (iter (for m in ms)
                                             (setf (gethash next-coord grid) m)
                                             (setf (gethash (aref other 4) used) t)
                                             (rec m next-coord (mod (+ side-number diff) 4))
                                             (remhash (aref other 4) used)
                                             (remhash next-coord grid))
                                       (until ms))))))))))
      (iter (for tile in tiles)
            (setf (gethash (aref tile 4) used) t)
            (format t "trying tile ~a~%" tile)
            (iter (for side-number from 0 to 3) 
                  (rec tile (cons 0 0) side-number))
            (remhash (aref tile 4) used)))))
