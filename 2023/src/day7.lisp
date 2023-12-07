(defpackage :day7
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind)
  (:export
   :card-counts
   :make-hand
   :has-lower-card
   :hand-is-less-than
   :determine-hand-type-with-jokers
   :rank-hands-with-jokers
   :parse-hands-from-file-with-jokers))

(in-package :day7)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct hand cards bid type)

(defun card-counts (cards)
  (loop with counts = (make-hash-table)
        for card across cards
        do (setf (gethash card counts) 
                 (+ (gethash card counts 0) 1))
        finally (return (sort (alexandria:hash-table-values counts) #'>))))

(defun determine-hand-type (cards)
  (cond ((equal (card-counts cards) (list 5)) 7)
        ((equal (card-counts cards) (list 4 1)) 6)
        ((equal (card-counts cards) (list 3 2)) 5)
        ((equal (card-counts cards) (list 3 1 1)) 4)
        ((equal (card-counts cards) (list 2 2 1)) 3)
        ((equal (card-counts cards) (list 2 1 1 1)) 2)
        ((equal (card-counts cards) (list 1 1 1 1 1)) 1)))

(defun parse-hand ()
  (sequential (cards (manyn #'alphanumericp 5))
              (_ (many1 #'whitespacep))
              (bid *non-negative-int*)
              (make-hand :cards cards :bid bid :type (determine-hand-type cards))))

(defun parse-hands ()
  (sep-by (parse-hand) (many1 #'newlinep)))

(defun parse-hands-from-file (filename)
  (parse-file filename (parse-hands)))

(defun card-value (card)
  (case card
    (#\A 14)
    (#\K 13)
    (#\Q 12)
    (#\J 11)
    (#\T 10)
    (t (digit-char-p card))))

(defun has-lower-card (one other)
  (loop for c1 across one
        for c2 across other
        for value1 = (card-value c1)
        for value2 = (card-value c2)
        when (< value1 value2)
          do (return t)
        when (> value1 value2)
          do (return nil)
        finally (return nil)))

(defun hand-is-less-than (one other)
  (or (< (hand-type one) (hand-type other))
      (and (= (hand-type one) (hand-type other))
           (has-lower-card (hand-cards one) (hand-cards other)))))

(defun rank-hands (hands)
  (sort hands #'hand-is-less-than))

(defun total-winnings (hands)
  (loop for i from 1
        for hand in (rank-hands hands)
        summing (* i (hand-bid hand))))

(defun part1 ()
  (total-winnings (parse-hands-from-file "input7")))

(defun count-cards-with-joker (cards)
  (loop with counts = (make-hash-table)
        with jokers = 0
        for card across cards
        when (char/= card #\J)
          do (setf (gethash card counts)
                   (+ (gethash card counts 0) 1))
        when (char= card #\J)
          do (incf jokers)
        finally (return (let ((counts (sort (alexandria:hash-table-values counts) #'>)))
                          (if (null counts)
                              (list 5)
                              (cons (+ (car counts) jokers) (cdr counts)))))))

(defun determine-hand-type-with-jokers (cards)
  (cond ((equal (count-cards-with-joker cards) (list 5)) 7)
        ((equal (count-cards-with-joker cards) (list 4 1)) 6)
        ((equal (count-cards-with-joker cards) (list 3 2)) 5)
        ((equal (count-cards-with-joker cards) (list 3 1 1)) 4)
        ((equal (count-cards-with-joker cards) (list 2 2 1)) 3)
        ((equal (count-cards-with-joker cards) (list 2 1 1 1)) 2)
        ((equal (count-cards-with-joker cards) (list 1 1 1 1 1)) 1)))

(defun parse-hand-with-jokers ()
  (sequential (cards (manyn #'alphanumericp 5))
              (_ (many1 #'whitespacep))
              (bid *non-negative-int*)
              (make-hand :cards cards :bid bid :type (determine-hand-type-with-jokers cards))))

(defun parse-hands-with-jokers ()
  (sep-by (parse-hand-with-jokers) (many1 #'newlinep)))

(defun parse-hands-from-file-with-jokers (filename)
  (parse-file filename (parse-hands-with-jokers)))

(defun card-value-with-jokers (card)
  (case card
    (#\A 14)
    (#\K 13)
    (#\Q 12)
    (#\J 0)
    (#\T 10)
    (t (digit-char-p card))))

(defun has-lower-card-with-jokers (one other)
  (loop for c1 across one
        for c2 across other
        for value1 = (card-value-with-jokers c1)
        for value2 = (card-value-with-jokers c2)
        when (< value1 value2)
          do (return t)
        when (> value1 value2)
          do (return nil)
        finally (return nil)))

(defun hand-is-less-than-with-jokers (one other)
  (or (< (hand-type one) (hand-type other))
      (and (= (hand-type one) (hand-type other))
           (has-lower-card-with-jokers (hand-cards one) (hand-cards other)))))

(defun rank-hands-with-jokers (hands)
  (sort hands #'hand-is-less-than-with-jokers))

(defun total-winnings-with-jokers (hands)
  (loop for i from 1
        for hand in (rank-hands-with-jokers hands)
        summing (* i (hand-bid hand))))

(defun part2 ()
  (total-winnings-with-jokers (parse-hands-from-file-with-jokers "input7")))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
