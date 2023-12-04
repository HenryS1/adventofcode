(defpackage :day4
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind)
  (:export 
   :parse-card
   :card-id
   :card-have
   :card-winning
   :card-worth
   :total-worth
   :parse-all-cards
   :win-copies
   :total-winnings))

(in-package :day4)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct card id winning have)

(defun parse-numbers ()
  (sep-by *non-negative-int* (many1 #p(char= #\space))))

(defun ignore-whitespace () (many #p(char= #\space)))

(defun parse-card ()
  (sequential (_ (seq "Card"))
              (_ (ignore-whitespace))
              (id *positive-int*)
              (_ (char1 #\:))
              (_ (ignore-whitespace))
              (winning (parse-numbers))
              (_ (ignore-whitespace))
              (_ (seq "|"))
              (_ (ignore-whitespace))
              (have (parse-numbers))
              (make-card :id id :winning winning :have have)))

(defun card-worth (card)
  (let* ((in-both (intersection (card-winning card) (card-have card)))
         (number-in-both (length in-both)))
    (if (= number-in-both 0) 0 (expt 2 (- number-in-both 1)))))

(defun parse-all-cards (filename)
  (parse-file filename (sep-by (parse-card) (many #'newlinep))))

(defun total-worth (filename)
  (reduce #'+ (mapcar #'card-worth (parse-all-cards filename))))

(defun part1 ()
  (total-worth "input4"))

(defun win-copies (card remaining cache)
  (let ((number-in-both (length (intersection (card-winning card) (card-have card)))))
    (or (gethash (card-id card) cache)
        (let ((total-copies (loop for i from 1 to number-in-both
                                  for other-card in remaining
                                  for new-remaining = (cdr remaining) then (cdr new-remaining)
                                  summing (+ 1 (win-copies other-card new-remaining cache)))))
          (setf (gethash (card-id card) cache) total-copies)))))

(defun total-winnings (cards)
  (+ (length cards)
     (loop with cache = (make-hash-table)
           for card in cards
           for remaining = (cdr cards) then (cdr remaining)
           summing (win-copies card remaining cache))))

(defun part2 ()
  (let ((cards (parse-file "input4" (sep-by (parse-card) (many #'newlinep)))))
    (total-winnings cards)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
