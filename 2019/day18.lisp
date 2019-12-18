(load "../2018/queue.lisp")
(load "../2018/priority-queue.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(declaim (optimize (debug 3) (speed 0)))

(defpackage :day18
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day18)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-map ()
  (map 'vector #'identity 
       (iter (for line in-file "input18" using #'read-line)
             (collect line))))

(defun map-at (mp r c)
  (aref (aref mp r) c))

(defun rows (mp)
  (length mp))

(defun cols (mp) 
  (length (aref mp 0)))

(defun find-keys ()
  (iter outer
        (for line in-file "input18" using #'read-line)
        (iter (for c in-string line)
              (when (and (lower-case-p c) (alphanumericp c))
                (in outer (collect c))))))

(defun in-bounds (candidate mp)
  (and (<= 0 (car candidate) (- (rows mp) 1))
       (<= 0 (cdr candidate) (- (cols mp) 1))))

(defun eligible (mp unlocked)
  (lambda (candidate)
   (and (in-bounds candidate mp)
        (let ((sq (map-at mp (car candidate) (cdr candidate))))
          (or 
           (char= sq #\.)
           (lower-case-p sq)
           (and (upper-case-p sq) 
                (find (char-downcase sq) unlocked)))))))

(defun neighbours (mp unlocked r c)
  (let ((candidates (list (cons (+ r 1) c) (cons (- r 1) c) 
                          (cons r (+ c 1)) (cons r (- c 1)))))
    (remove-if-not (eligible mp unlocked) candidates)))

(defun explore (coord mp moves-so-far unlocked)
  (iter (with q = (make-queue (cons coord 0)))
        (with seen = (alist-hash-table (list (cons coord t)) :test 'equal))
        (with keys-seen = (list))
        (while (not (empty q)))
        (for blah = (peek q))
        (for ((r . c) . moves-to) = (poll q))
        (when (lower-case-p (map-at mp r c))
          (push (cons (cons r c) (+ moves-to moves-so-far)) keys-seen))
        (iter (for nbr in (neighbours mp unlocked r c))
              (when (not (gethash nbr seen))
                (setf (gethash nbr seen) t)
                (enqueue (cons nbr (+ moves-to 1)) q)))
        (finally (return keys-seen))))

(defun key-locations (mp)
  (iter outer 
        (for row in-vector mp)
        (for r from 0)
        (iter (for c index-of-vector row)
              (when (lower-case-p (map-at mp r c))
                (in outer (collect (cons (map-at mp r c) (cons r c))))))))

(defun priority (key-locations)
  (lambda (one)
    (bind (((unlocked moves-to _) one))
      (+ moves-to (- (length key-locations) (length unlocked))))))

(defun find-start-coord (mp)
  (iter outer
        (for row in-vector mp)
        (for r from 0)
        (iter (for c index-of-vector row)
              (when (char= (map-at mp r c) #\@)
                (return-from outer (cons r c))))))

(defun find-best-path (mp)
  (let* ((start-coord (find-start-coord mp))
         (key-locs (key-locations mp))
         (pq (make-pq (priority key-locs))))
    (insert-pq (list (list) 0 start-coord))
    (iter (while (not (pq-empty pq)))
          (for (unlocked moves-so-far coord) = (pop-pq pq))
          (iter (for ())))))
