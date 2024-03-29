(defpackage :day12
  (:use 
   :cl
   :cl-ppcre
   :trivia
   :trivia.ppcre 
   :iterate 
   :alexandria 
   :anaphora
   :metabang-bind
   :aoc.datastructures))

(in-package :day12)

(defun read-lines ()
  (coerce (iter (for line in-file "day12.input" using #'read-line)
     (collect line)) 'vector))

(defun traversable (current-char dest-char)
  (let ((current-char-code 
          (if (char= current-char #\S) (char-code #\a) (char-code current-char)))
        (dest-char-code (if (char= dest-char #\E) (char-code #\z) (char-code dest-char))))
    (<= (- dest-char-code current-char-code) 1)))

(defun neighbours (current mp)
  (bind ((rows (length mp))
         (cols (length (aref mp 0)))
         ((r . c) current)
         (current-char (aref (aref mp r) c))
         (left (and (> c 0) (aref (aref mp r) (- c 1))))
         (right (and (< c (- cols 1)) (aref (aref mp r) (+ c 1))))
         (up (and (> r 0) (aref (aref mp (- r 1)) c)))
         (down (and (< r (- rows 1)) (aref (aref mp (+ r 1)) c))))
    (remove-if-not #'identity 
               (list (and left (traversable current-char left) (cons r (- c 1)))
                     (and right (traversable current-char right) (cons r (+ c 1)))
                     (and up (traversable current-char up) (cons (- r 1) c))
                     (and down (traversable current-char down) (cons (+ r 1) c))))))

(defun find-start (mp)
  (let ((rows (length mp))
        (cols (length (aref mp 0))))
    (labels ((rec (r c)
               (cond ((= r rows) nil)
                     ((= c cols) (rec (+ r 1) 0))
                     ((char= (aref (aref mp r) c) #\S)
                      (cons r c))
                     (t (rec r (+ c 1))))))
      (rec 0 0))))

(defun bfs (start mp)
  (let ((seen (make-hash-table :test 'equal)))
    (setf (gethash start seen) t)
    (labels ((rec (q seen)
               (if (q-empty q)
                   nil
                   (bind ((((e . steps) . new-q) (dequeue q))
                          ((r . c) e)
                          (current-char (aref (aref mp r) c)))
                     (if (char= current-char #\E) 
                         steps
                         (let ((nbrs (remove-if (lambda (n) (gethash n seen)) (neighbours e mp))))
                           (mapc (lambda (n) (setf (gethash n seen) t)) nbrs)
                           (rec (enqueue-all (mapcar (lambda (n) (cons n (+ steps 1))) nbrs) new-q)
                                seen)))))))
      (rec (make-queue (list (cons start 0))) seen))))

(defun part1 ()
  (bind ((mp (read-lines))
         (start (find-start mp)))
    (bfs start mp)))

(defun replace-s (mp)
  (map 'vector (lambda (s) (map 'string (lambda (c) (if (char= c #\S) #\a c)) s)) mp))

(defun find-starts (mp)
  (let ((rows (length mp))
        (cols (length (aref mp 0))))
    (labels ((rec (r c starts)
               (cond ((= r rows) starts)
                     ((= c cols) (rec (+ r 1) 0 starts))
                     ((char= (aref (aref mp r) c) #\a)
                      (rec r (+ c 1) (cons (cons r c) starts)))
                     (t (rec r (+ c 1) starts)))))
      (rec 0 0 nil))))

(defun part2 ()
  (let* ((mp (replace-s (read-lines)))
         (starts (find-starts mp)))
    (reduce #'min (remove-if-not #'identity (mapcar (lambda (s) (bfs s mp)) starts)))))
