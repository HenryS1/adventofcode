(defpackage :day10
  (:use :cl :cl-ppcre :trivia trivia.ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day10)

(defun read-lines ()
  (iter (for line in-file "day10.input" using #'read-line)
    (collect (match line
               ((ppcre "addx (-?\\d+)" (read delta))
                (list 'addx delta))
               ((ppcre "noop") (list 'noop))))))

(defun signal-strength (lines x cycles checkpoints)
  (when checkpoints
    (bind (((new-x . new-lines)
            (match (car lines)
              ((list 'noop) (cons x (cdr lines)))
              ((list 'busy delta) (cons (+ x delta) (cdr lines)))
              ((list 'addx delta) (cons x (cons (list 'busy delta) (cdr lines)))))))
      (if (equal cycles (car checkpoints))
          (cons (* cycles x) 
                (signal-strength new-lines new-x (+ cycles 1) (cdr checkpoints)))
          (signal-strength new-lines new-x (+ cycles 1) checkpoints)))))

(defun part1 ()
  (reduce #'+ (signal-strength (read-lines) 1 1 (list 20 60 100 140 180 220))))

(defun take-drop (n seq)
  (cons (subseq seq 0 n)
        (subseq seq n)))

(defun draw (lines x cycle pixels)
  (when lines
    (bind (((new-x . new-lines)
            (match (car lines)
              ((list 'noop) (cons x (cdr lines)))
              ((list 'busy delta) (cons (+ x delta) (cdr lines)))
              ((list 'addx delta) (cons x (cons (list 'busy delta) (cdr lines))))))
           (cursor-position (mod cycle 40)))
      (iter (for i from x to (+ x 2))
        (when (= i cursor-position)
          (setf (aref pixels cycle) #\#)))
      (draw new-lines new-x (+ cycle 1) pixels))))

(defun split-lines (pixels)
  (if (= (length pixels) 0)
      nil
      (bind (((fst . rest) (take-drop 40 pixels)))
        (cons fst (split-lines rest)))))

(defun take (n l)
  (if (or (null l) (= n 0)) nil (cons (car l) (take (- n 1) (cdr l)))))

(defun part2 ()
  (let ((pixels (make-string 240 :initial-element #\.)))
    (draw (read-lines) 0 0 pixels)
    (split-lines pixels)))
