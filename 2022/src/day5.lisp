(defpackage :day5
  (:use :cl :cl-ppcre :trivia trivia.ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day5)

(defun read-lines ()
  (iter (for line in-file "day5.input" using #'read-line)
    (collect line)))

(defun transpose-crates (crates-rows)
  (if (null (car crates-rows))
      nil
      (cons (mapcar #'car crates-rows) 
            (transpose-crates (mapcar #'cdr crates-rows)))))

(defun crates (lines)
  (mapcar (lambda (col) (remove-if (lambda (s) (string= s "   ")) col))
          (transpose-crates (iter (for line in lines)
                              (for row = 
                                   (match line
                                     ((ppcre "(   |\\[\\w\\]) (   |\\[\\w\\]) (   |\\[\\w\\]) (   |\\[\\w\\]) (   |\\[\\w\\]) (   |\\[\\w\\]) (   |\\[\\w\\]) (   |\\[\\w\\]) (   |\\[\\w\\])"
                                             c1 c2 c3 c4 c5 c6 c7 c8 c9)
                                      (vector c1 c2 c3 c4 c5 c6 c7 c8 c9))
                                     (t nil)))
                              (while row)
                              (collect (coerce row 'list))))))

(defun moves (lines)
  (iter (for line in lines)
    (for move = (match line
                  ((ppcre "move (\\d+) from (\\d+) to (\\d+)"
                          (read number) (read start) (read end))
                   (list number (- start 1) (- end 1)))))
    (when move (collect move))))

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
  (bind ((lines (read-lines))
         (moves (moves lines))
         (crates (crates lines)))
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
  (bind ((lines (read-lines))
         (moves (moves lines))
         (crates (crates lines)))
    (mapcar #'car (make-moves-in-order crates moves))))
