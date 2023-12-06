(defpackage day6-tests
  (:use :cl :day6 :parachute :pears))

(in-package :day6-tests)

(define-test day6-suite)

(define-test distances-for-race-finds-all-distances-that-can-be-travelled
  :parent day6-suite
  (let ((race (make-race :time 7 :distance 9)))
    (is equalp 
        (list 0 6 10 12 12 10 6 0)
        (distances-for-race race))))

(define-test number-of-ways-to-beat-record-counts-ways-to-beat-the-record
  :parent day6-suite
  (let ((race (make-race :time 7 :distance 9)))
    (is = 4 (number-of-ways-to-beat-record race))))

(define-test margin-of-error-is-the-product-of-the-ways-to-beat-the-record
  :parent day6-suite
  (let ((races (parse-races-from-file "../tests/test-input6")))
    (is = 288 (margin-of-error races))))

(define-test find-first-time-record-is-broken-finds-first-eligible-time
  :parent day6-suite
  (is = 14 (find-first-time-record-is-broken 71530 940200)))

(define-test find-last-time-record-is-broken-finds-last-eligible-time
  :parent day6-suite
  (is = 71516 (find-last-time-record-is-broken 71530 940200)))

