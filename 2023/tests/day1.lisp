(defpackage day1-tests
  (:use :cl :day1 :parachute :pears))

(in-package :day1-tests)

(define-test day1-suite)

(define-test calibration-value-is-number-made-from-first-and-last-digit
  :parent day1-suite
  (is = (calibration-value "1a3bc2") 12)
  (is = (calibration-value "yw3kjahw6kjashd9asjdhg") 39))

(define-test total-calibration-value-is-sum-of-calibration-values
  :parent day1-suite
  (is = (total-calibration-value (list "1a3bc2" "yw3kjahw6kjashd9asjdhg")) 51))

(define-test digits-in-line-finds-all-digits-in-a-line
  :parent day1-suite
  (is equal (digits-in-line "one") (list 1))
  (is equal (digits-in-line "2one") (list 2 1))
  (is equal (digits-in-line "twoneight") (list 2 1 8)))

(define-test all-calibration-values-finds-all-calibration-values-across-lines
  :parent day1-suite
  (is equal (parse-string (all-calibration-values) "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen") 
      (list 29 83 13 24 42 14 76)))
