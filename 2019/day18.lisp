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

(defun eligible-full (mp)
  (lambda (candidate)
   (and (in-bounds candidate mp)
        (let ((sq (map-at mp (car candidate) (cdr candidate))))
          (or 
           (char= sq #\.)
           (alpha-char-p sq))))))

(defun neighbours-full (mp r c)
  (let ((candidates (list (cons (+ r 1) c) (cons (- r 1) c) 
                          (cons r (+ c 1)) (cons r (- c 1)))))
    (remove-if-not (eligible-full mp) candidates)))

(defun explore (coord mp moves-so-far unlocked)
  (iter (with q = (make-queue (cons coord 0)))
        (with seen = (alist-hash-table (list (cons coord t)) :test 'equal))
        ;(with keys-seen = (list))
        (while (not (empty q)))
        (for blah = (peek q))
        (for ((r . c) . moves-to) = (poll q))
        (when (and (lower-case-p (map-at mp r c)) 
                   (not (find (map-at mp r c) unlocked)))
          (collect (cons (cons r c) (+ moves-to moves-so-far)) into keys-seen))
        (iter (for nbr in (neighbours mp unlocked r c))
              (when (not (gethash nbr seen))
                (setf (gethash nbr seen) t)
                (enqueue (cons nbr (+ moves-to 1)) q)))
        (finally (return keys-seen))))

(defun edges-from (coord mp)
  (iter outer
        (with q = (make-queue (cons coord 0)))
        (with seen = (alist-hash-table (list (cons coord t)) :test 'equal))
        (while (not (empty q)))
        (for ((r . c) . moves-to) = (poll q))       
        (iter (for nbr in (neighbours-full mp r c))
              (for sq = (map-at mp (car nbr) (cdr nbr)))
              (when (not (gethash nbr seen))
                (setf (gethash nbr seen) t)
                (when (alpha-char-p sq)
                  (in outer (collect (cons nbr (+ moves-to 1)) into keys-seen)))
                (when (not (upper-case-p sq))
                  (enqueue (cons nbr (+ moves-to 1)) q))))
        (finally (return-from outer keys-seen))))

(defun key-locations (mp)
  (iter outer 
        (for row in-vector mp)
        (for r from 0)
        (iter (for c index-of-vector row)
              (when (lower-case-p (map-at mp r c))
                (in outer (collect (cons (map-at mp r c) (cons r c))))))))

(defun locations (mp)
  (iter outer 
        (for row in-vector mp)
        (for r from 0)
        (iter (for c index-of-vector row)
              (when (alpha-char-p (map-at mp r c))
                (in outer (collect (cons r c) into result))))
        (finally (return-from outer (cons (find-start-coord mp) result)))))

(defun total-distance-from (coord key-locations)
  (iter (with (r . c) = coord)
        (for (sq . (r-kl . c-kl)) in key-locations)
        (sum (+ (abs (- r-kl r))  (abs (- c-kl c))))))

(defun make-graph (mp)
  (iter (with graph = (make-hash-table :test 'equal))
        (for loc in (locations mp))
        (setf (gethash loc graph) (edges-from loc mp))
        (finally (return graph))))

(defun priority (key-locations one)
  (bind (((unlocked moves-to coord) one))
    (let ((unseen (remove-if (lambda (e) (find (car e) unlocked)) key-locations)))
      (+ moves-to (total-distance-from coord unseen)))))

(defun distance-to-goal (key-locations)
  (lambda (one other)
    (< (priority key-locations one) (priority key-locations other))))

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
         (pq (make-pq (distance-to-goal key-locs))))
    (insert-pq (list (list) 0 start-coord) pq)
    (iter (while (not (pq-empty pq)))
          (for (unlocked moves-so-far coord) = (pop-pq pq))
          (format t "UNLOCKED ~a MOVES-SO-FAR ~a~%" unlocked moves-so-far)
          (until (= (length key-locs) (length unlocked)))
          (iter (for (key-coord . moves-to) in (explore coord mp moves-so-far 
                                                        unlocked))
                (insert-pq (list (cons (map-at mp (car key-coord) (cdr key-coord)) unlocked)
                               (+ moves-so-far moves-to)
                               key-coord) pq))
          (finally (return moves-so-far)))))

(defun explore-to-end (mp)
  (let ((start-coord (find-start-coord mp))
        (target-len (length (key-locations mp)))
        (best-total 100000000000))
    (labels ((rec (coord moves-so-far unlocked)
               (format t "UNLOCKED ~a~%" unlocked)
               ;; (format t "COORD ~a CHAR ~a UNLOCKED ~a MOVES ~a~%"
               ;;         coord (map-at mp (car coord) (cdr coord)) unlocked moves-so-far)
               (if (= (length unlocked) target-len)
                   (when (< moves-so-far best-total)
                     (format t "NEW BEST ~a~%" moves-so-far)
                     (setf best-total moves-so-far))
                   (iter (for (next-coord . moves-to) 
                              in (explore coord mp moves-so-far unlocked))
                         (when (< (+ moves-so-far moves-to) best-total)
                           (rec next-coord (+ moves-so-far moves-to) 
                                (cons (map-at mp (car next-coord) (cdr next-coord))
                                      unlocked)))))))
      (rec start-coord 0 nil))))

(defun find-reachable (start graph mp unlocked)
;  (format t "UNLOCKED ~a~%" unlocked)
  (iter (with pq = (let ((q (make-pq (lambda (one other) (< (car one) (car other))))))
                     (insert-pq (cons 0 start) q) q))
        (with distance-to = (make-hash-table :test 'equal))
        (while (not (pq-empty pq)))
        (for (moves-to . coord) = (pop-pq pq))
;        (format t "COORD ~a~%" coord)
        (when (not (gethash coord distance-to))
;          (format t "HERE~%")
          (setf (gethash coord distance-to) moves-to)
          (iter (for (nbr . dist) in (gethash coord graph))
                (for sq = (map-at mp (car nbr) (cdr nbr)))
;                (format t "NBR ~a SQ ~a~%" nbr sq)
                (when (and (or (lower-case-p sq) 
                               (find (char-downcase sq) unlocked)))
                  (insert-pq (cons (+ moves-to dist) nbr) pq))))
        (finally (return (remove-if-not (lambda (e) 
                                          (let ((sq (map-at mp (caar e) (cdar e))))
                                            (and (not (find sq unlocked))
                                                 (lower-case-p sq))))
                                        (hash-table-alist distance-to))))))

(defun find-reachable-estimate (start graph mp unlocked)
;  (format t "UNLOCKED ~a~%" unlocked)
  (iter (with pq = (let ((q (make-pq (lambda (one other) (< (car one) (car other))))))
                     (insert-pq (cons 0 start) q) q))
        (with distance-to = (make-hash-table :test 'equal))
        (while (not (pq-empty pq)))
        (for (moves-to . coord) = (pop-pq pq))
;        (format t "COORD ~a~%" coord)
        (when (not (gethash coord distance-to))
;          (format t "HERE~%")
          (setf (gethash coord distance-to) moves-to)
          (iter (for (nbr . dist) in (gethash coord graph))
                (for sq = (map-at mp (car nbr) (cdr nbr)))
;                (format t "NBR ~a SQ ~a~%" nbr sq)
                (when (and (or (lower-case-p sq) 
                               (find (char-downcase sq) *all*)))
                  (insert-pq (cons (+ moves-to dist) nbr) pq))))
        (finally (return (remove-if-not (lambda (e) 
                                          (let ((sq (map-at mp (caar e) (cdar e))))
                                            (and (not (find sq unlocked))
                                                 (lower-case-p sq))))
                                        (hash-table-alist distance-to))))))

(defparameter *all* '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s
 #\t #\u #\v #\w #\x #\y #\z))

(defun estimated-moves-remaining (coord key-locations unlocked)
  (iter (for (sq . (r . c)) in key-locations)
        (when (not (find sq unlocked))
          (sum (+ (abs (- (car coord) r)) (abs (- (cdr coord) c)))))))

(defun sharp-estimated-moves-remaining (coord graph mp unlocked)
  (iter (for (crd . mvs-to) in (find-reachable-estimate coord graph mp unlocked))
        (sum (+ (abs (- (car crd) (car coord))) (abs (- (cdr crd) (cdr coord)))))))

(defun explore-compressed (mp)
  (let* ((graph (make-graph mp))
         (kls (key-locations mp))
         (target-len (length kls))
         (best-total 100000000000))
    (labels ((rec (coord moves-so-far unlocked)
;               (format t "UNLOCKED ~a~%" unlocked)
               (if (= (length unlocked) target-len)
                   (when (< moves-so-far best-total)
                     (format t "NEW BEST ~a~%" moves-so-far)
                     (setf best-total moves-so-far))
                   (iter (with reachable = (sort (find-reachable coord graph mp unlocked)
                                                 (lambda (one other) (< (cdr one) (cdr other)))))
;                         (format t "REACHABLE ~a~%" reachable)
                         (for (next-coord . moves-to) in reachable)
;                         (format t "NEXT COORD ~a~%" next-coord)
                         (when (< (+ (+ moves-so-far moves-to)
                                     (estimated-moves-remaining next-coord kls unlocked))
                                  best-total)
                           (rec next-coord (+ moves-so-far moves-to)
                                (cons (map-at mp (car next-coord) (cdr next-coord)) 
                                      unlocked)))))
;               (format t "COORD ~a~%" coord)
;               (format t "UNLOCKED ~a~%" unlocked)
               ))
      (rec (find-start-coord mp) 0 nil))))

(defun factorial (n)
  (if (< n 2) 1 (* n (factorial (- n 1)))))
