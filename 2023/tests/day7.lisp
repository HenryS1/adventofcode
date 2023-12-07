(defpackage day7-tests
  (:use :cl :day7 :parachute :pears))

(in-package :day7-tests)

(define-test day7-suite)

(define-test card-counts-returns-card-counts-in-descending-order
  :parent day7-suite
  (is equal (card-counts "AAAAA") (list 5))
  (is equal (card-counts "AA8AA") (list 4 1))
  (is equal (card-counts "23332") (list 3 2))
  (is equal (card-counts "TTT98") (list 3 1 1))
  (is equal (card-counts "23432") (list 2 2 1))
  (is equal (card-counts "A23A4") (list 2 1 1 1))
  (is equal (card-counts "23456") (list 1 1 1 1 1)))

(define-test has-lower-card-returns-true-if-hand-loses-based-on-high-card
  :parent day7-suite
  (true (has-lower-card "KTJJT" "KK677"))
  (true (has-lower-card "T55J5" "QQQJA")))


(define-test determine-hand-type-with-jokers
  :parent day7-suite
  (is equal (determine-hand-type-with-jokers "KTJJT") 6))

(define-test rank-hands-with-jokers-uses-jokers-to-improve-hands-when-available
  :parent day7-suite
  (is equalp 
      (list (make-hand :cards "32T3K" :bid 765 :type 2)
            (make-hand :cards "KK677" :bid 28 :type 3)
            (make-hand :cards "T55J5" :bid 684 :type 6)
            (make-hand :cards "QQQJA" :bid 483 :type 6)
            (make-hand :cards "KTJJT" :bid 220 :type 6))
      (rank-hands-with-jokers (parse-hands-from-file-with-jokers "../tests/test-input7"))))
