(defpackage :day5
  (:use :cl :iterate :alexandria :anaphora :metabang-bind :pears))

(in-package :day5)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-empty ()
  (sequential (_ (seq "   ")) nil))

(defun parse-crate ()
  (sequential (_ (char1 #\[))
              (crate (fmap #p(intern) (anyn 1)))
              (_ (char1 #\]))
              crate))

(defun parse-row-entry ()
  (orp (parse-empty)
       (parse-crate)))

(defun parse-row ()
  (sequential (entries (sep-by (parse-row-entry) (char1 #\space)))
              (_ (char1 #\newline))
              entries))

(defun ignore-line ()
  (sequential (_ (many #l(not (newlinep %c)))) (_ (char1 #\newline)) nil))

(defun transpose-crates (crates-rows)
  (if (null (car crates-rows))
      nil
      (cons (mapcar #'car crates-rows) 
            (transpose-crates (mapcar #'cdr crates-rows)))))

(defun parse-columns ()
  (sequential (columns (fmap #l(coerce %rows 'list) (repeated (parse-row))))
              (_ (ignore-line))
              (mapcar #p(remove-if #'null) (transpose-crates columns))))

(defun parse-move ()
  (sequential (_ (seq "move "))
              (number *positive-int*)
              (_ (seq " from "))
              (start *positive-int*)
              (_ (seq " to "))
              (end *positive-int*)
              (list number (- start 1) (- end 1))))

(defun parse-moves ()
  (sep-by (parse-move) (char1 #\newline)))

(defun parse-lines ()
  (parse-file "day5.input" 
              (sequential (columns (parse-columns))
                          (_ (ignore-line))
                          (moves (parse-moves))
                          (cons columns moves))))

(defun update-at (n new cols)
  (if (= n 0)
      (cons new (cdr cols))
      (cons (car cols) (update-at (- n 1) new (cdr cols)))))

(defun drop (n l)
  (if (= n 0) l (drop (- n 1) (cdr l))))

(defun take (n l)
  (if (or (= n 0) (null l)) nil (cons (car l) (take (- n 1) (cdr l)))))

(defun move-crates (n start end cols)
  (let ((start-col (nth start cols)))
    (update-at end (append (reverse (take n start-col)) (nth end cols))
               (update-at start (drop n start-col) cols))))

(defun make-moves (cols moves)
  (if (null moves)
      cols
      (bind (((n start end) (car moves)))
        (make-moves (move-crates n start end cols) (cdr moves)))))

(defun part1 ()
  (bind (((crates . moves) (parse-lines)))
    (mapcar #'car (make-moves crates moves))))

(defun move-crates-in-order (n start end cols)
  (let ((start-col (nth start cols)))
    (update-at end (append (take n start-col) (nth end cols))
               (update-at start (drop n start-col) cols))))

(defun make-moves-in-order (cols moves)
  (if (null moves)
      cols 
      (bind (((n start end) (car moves)))
        (make-moves-in-order (move-crates-in-order n start end cols) (cdr moves)))))

(defun part2 ()
  (bind (((crates . moves) (parse-lines)) )
    (mapcar #'car (make-moves-in-order crates moves))))
