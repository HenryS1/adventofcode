(defpackage day4-tests
  (:use :cl :day4 :parachute :pears))

(in-package :day4-tests)

(define-test day4-suite)

(define-test parse-card-parses-id-winning-and-numbers-you-have
  :parent day4-suite
  (let ((card (parse-string (parse-card) "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")))
    (is = (card-id card) 1)
    (is equal (card-winning card) (list 41 48 83 86 17))
    (is equal (card-have card) (list 83 86 6 31 17 9 48 53))))

(define-test card-worth-finds-the-worth-of-a-card 
  :parent day4-suite
  (let ((card (parse-string (parse-card) "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")))
    (is = (card-worth card) 8)))

(define-test total-worth-finds-the-total-worth-for-cards-in-a-file
  :parent day4-suite
  (is = (total-worth "../tests/test-input4") 13))

(define-test win-copies-finds-the-number-of-copies-generated-by-a-card
  :parent day4-suite
  (let* ((cards (parse-all-cards "../tests/test-input4")))
    (is = 14 (win-copies (car cards) (cdr cards) (make-hash-table)))))

(define-test total-winnings-finds-the-overall-winnings
  :parent day4-suite
    (let* ((cards (parse-all-cards "../tests/test-input4")))
      (is = 30 (total-winnings cards))))
