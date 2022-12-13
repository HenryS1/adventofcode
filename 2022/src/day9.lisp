(defpackage :day9
  (:use :cl :cl-ppcre :trivia trivia.ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day9)

(defun read-lines ()
  (iter (for line in-file "day9.input" using #'read-line)
    (collect (match line
               ((ppcre "(\\w) (\\d+)"
                       (read dir) (read steps))
                (cons dir steps))))))

(defparameter *up* #c(0 1))
(defparameter *down* #c(0 -1))
(defparameter *right* #c(1 0))
(defparameter *left* #c(-1 0))

(defun tail-direction (hd tl)
  (let ((diff (- hd tl)))
    (case diff
      (#c(2 0) #c(1 0))
      (#c(-2 0) #c(-1 0))
      (#c(0 2) #c(0 1))
      (#c(0 -2) #c(0 -1))
      (#c(1 2) #c(1 1))
      (#c(1 -2) #c(1 -1))
      (#c(-1 2) #c(-1 1))
      (#c(-1 -2) #c(-1 -1))
      (#c(2 1) #c(1 1))
      (#c(2 -1) #c(1 -1))
      (#c(-2 1) #c(-1 1))
      (#c(-2 -1) #c(-1 -1))
      (#c(2 2) #c(1 1))
      (#c(2 -2) #c(1 -1))
      (#c(-2 2) #c(-1 1))
      (#c(-2 -2) #c(-1 -1))
      (t 0))))

(defun move-in-dir (hd tl dir count seen)
  (if (= count 0)
      (list hd tl seen)
      (let* ((diff (case dir
                     (U *up*)
                     (D *down*)
                     (L *left*)
                     (R *right*)
                     (t 0)))
             (new-h (+ diff hd))
             (new-t (+ (tail-direction new-h tl) tl)))
        (move-in-dir new-h new-t dir (- count 1) (adjoin new-t seen)))))

(defun move (steps)
  (labels ((rec (steps seen hd tl)
             (if (null steps)
                 seen
                 (bind (((new-h new-t new-seen) 
                         (move-in-dir hd tl (caar steps) (cdar steps) seen)))
                   (rec (cdr steps) new-seen new-h new-t)))))
    (rec steps (list 0) 0 0)))

(defun part1 ()
  (length (move (read-lines))))


(defun move-all (es diff)
  (if (null (cdr es))
      (list (+ (car es) diff))
      (let* ((new-e (+ (car es) diff))
             (new-diff (tail-direction new-e (cadr es))))
        (cons new-e (move-all (cdr es) new-diff)))))

(defun move-all-count (es dir count seen)
  (if (= count 0)
      (cons es seen)
      (let* ((init-diff (case dir 
                          (U *up*)
                          (D *down*) 
                          (L *left*)
                          (R *right*)
                          (t 0)))
             (new-es (move-all es init-diff))
             (new-seen (adjoin (car (last new-es)) seen)))
        (move-all-count new-es dir (- count 1) new-seen))))

(defun move-chain (steps &key (init-chain (list 0 0 0 0 0 0 0 0 0 0)))
  (labels ((rec (steps es seen)
             (if (null steps)
                 seen
                 (bind (((new-es . new-seen) 
                         (move-all-count es (caar steps) (cdar steps) seen)))
                   (rec (cdr steps) new-es new-seen)))))
    (rec steps init-chain (list 0))))

(defun part2 ()
  (length (move-chain (read-lines))))
