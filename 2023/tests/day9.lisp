(defpackage day9-tests
  (:use :cl :day9 :parachute :pears))

(in-package :day9-tests)

(define-test day9-suite)

(define-test next-number-finds-the-next-number-in-a-sequence 
  :parent day9-suite
  (is = 28 (next-number (list 1 3 6 10 15 21)))
  (is = 68 (next-number (list 10 13 16 21 30 45))))

(define-test previous-number-finds-the-previous-number-in-a-sequence
  :parent day9-suite
  (is = 5 (previous-number (list 10 13 16 21 30 45)))
  (is = 0 (previous-number (list 1 3 6 10 15 21)))
  (is = -3 (previous-number (list 0 3 6 9 12 15 18))))




