(defpackage :day2
  (:use :cl :trivia :pears :iterate :alexandria :anaphora :metabang-bind))

(in-package :day2)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-move ()
  (fmap #p(intern) (manyn #l(member %c '(#\A #\B #\C #\X #\Y #\Z)) 1)))

(defun parse-moves ()
  (sep-by (sequential (opponent (parse-move))
                      (_ (char1 #\space))
                      (me (parse-move))
                      (cons opponent me))
          (char1 #\newline)))

(defun read-lines ()
  (parse-file "day2.input" (parse-moves)))

(defun outcome (strategy)
  (match strategy 
    ((cons 'A 'X) 'draw)
    ((cons 'A 'Y) 'win)
    ((cons 'A 'Z) 'loss)
    ((cons 'B 'X) 'loss)
    ((cons 'B 'Y) 'draw)
    ((cons 'B 'Z) 'win)
    ((cons 'C 'X) 'win)
    ((cons 'C 'Y) 'loss)
    ((cons 'C 'Z) 'draw)))

(defun score (strategy)
  (let ((result (outcome strategy)))
    (+ (case result 
         (win 6)
         (draw 3)
         (loss 0))
       (case (cdr strategy)
         (X 1)
         (Y 2)
         (Z 3)))))

(defun required-move (strategy)
  (match strategy
    ((cons 'A 'X) (cons 'A 'Z))
    ((cons 'A 'Y) (cons 'A 'X))
    ((cons 'A 'Z) (cons 'A 'Y))
    ((cons 'B 'X) (cons 'B 'X))
    ((cons 'B 'Y) (cons 'B 'Y))
    ((cons 'B 'Z) (cons 'B 'Z))
    ((cons 'C 'X) (cons 'C 'Y))
    ((cons 'C 'Y) (cons 'C 'Z))
    ((cons 'C 'Z) (cons 'C 'X))))

(defun part1 ()
  (reduce #'+ (mapcar #'score (read-lines))))

(defun part2 ()
  (reduce #'+ (mapcar #'score (mapcar #'required-move (read-lines)))))
