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

(defpackage :day12
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day12)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input12" using #'read-line)
        (collect line)))

(defun read-instruction (line)
  (match line
    ((ppcre "(\\w)(\\\d+)" (read dir) (read number))
     (cons dir number))))

(defun read-instructions (lines) (mapcar #'read-instruction lines))

(defun move (direction offset position)
  (match (cons direction position)
    ((cons 'N (cons r c)) (cons (- r offset) c))
    ((cons 'S (cons r c)) (cons (+ r offset) c))
    ((cons 'E (cons r c)) (cons r (+ c offset)))
    ((cons 'W (cons r c)) (cons r (- c offset)))))

(defun direction-to-degrees (direction)
  (case direction
    (E 0)
    (N 90)
    (W 180)
    (S 270)))

(defun degrees-to-direction (degrees)
  (case (mod (+ degrees 360) 360) 
    (0 'E)
    (90 'N)
    (180 'W)
    (270 'S)))

(defun turn (orientation degrees direction)
  (let ((sign (if (eq orientation 'L) 1 -1)))
    (-> direction (direction-to-degrees) (+ (* degrees sign)) (degrees-to-direction))))

(defun navigate (instruction direction position)
  (match instruction
    ((cons 'F offset) (cons (move direction offset position) direction))
    ((cons 'L degrees) (cons position (turn 'L degrees direction)))
    ((cons 'R degrees) (cons position (turn 'R degrees direction)))
    ((cons compass offset) (cons (move compass offset position) direction))))

(defun find-distance (lines)
  (iter (with instructions = (read-instructions lines))
        (for instruction in instructions)
        (for (position . direction) first (navigate instruction 'E (cons 0 0))
             then (navigate instruction direction position))
        (finally (return (+ (abs (car position)) (abs (cdr position)))))))

(defun rotate-waypoint (waypoint degrees orientation)
  (let ((sign (if (eq orientation 'L) 1 -1)))
    (match (cons (mod (+ 360 (* degrees sign)) 360) waypoint)
      ((cons 90 (cons r c)) (cons (- c) r))
      ((cons 180 (cons r c)) (cons (- r) (- c)))
      ((cons 270 (cons r c)) (cons c (- r))))))

(defun navigate-waypoint (instruction position waypoint)
  (match (list instruction waypoint position)
    ((list (cons 'F offset) (cons w1 w2) (cons p1 p2)) 
     (cons waypoint (cons (+ p1 (* w1 offset)) (+ p2 (* w2 offset)))))
    ((list (cons 'L degrees) _ _) (cons (rotate-waypoint waypoint degrees 'L) position))
    ((list (cons 'R degrees) _ _) (cons (rotate-waypoint waypoint degrees 'R) position))
    ((list (cons direction offset) _ _) (cons (move direction offset waypoint) position))))

(defun find-distance-waypoint (lines)
  (iter (with instructions = (read-instructions lines))
        (for instruction in instructions)
        (for (waypoint . position) first (navigate-waypoint instruction (cons 0 0) (cons -1 10))
             then (navigate-waypoint instruction position waypoint))
        (finally (return (+ (abs (car position)) (abs (cdr position)))))))
