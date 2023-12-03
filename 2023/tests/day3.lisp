(defpackage day3-tests
  (:use :cl :day3 :parachute :pears))

(in-package :day3-tests)

(define-test day3-suite)

(define-test number-length-is-one-for-zero
  :parent day3-suite
  (is = (number-length 0) 1))

(define-test number-length-is-the-count-of-digits-in-a-number
  :parent day3-suite
  (is = (number-length 5678) 4))

(define-test find-numbers-and-parts-finds-the-locations-of-numbers-and-parts
  :parent day3-suite
  (let* ((input-map (read-map-from-file "../tests/test-input3"))
         (part-map (find-numbers-and-parts input-map)))
    (is = 10 (hash-table-count (part-map-numbers part-map)))))

(define-test number-is-part-number-returns-true-when-number-is-adjacent-to-part
  :parent day3-suite
  (let ((input-map (read-map-from-file "../tests/test-input3"))
        (number-coord-467 (make-number-coord :row 0 :col 0 :length 3))
        (number-coord-755 (make-number-coord :row 7 :col 6 :length 3)))
    (true (is-part-number number-coord-467 input-map))
    (true (is-part-number number-coord-755 input-map))))

(define-test number-is-part-number-returns-false-when-number-is-not-adjacent-to-part
  :parent day3-suite
  (let ((input-map (read-map-from-file "../tests/test-input3"))
        (number-coord-114 (make-number-coord :row 0 :col 5 :length 3)))
    (false (is-part-number number-coord-114 input-map))))

(define-test find-part-numbers-returns-all-part-numbers-in-the-input-map
  :parent day3-suite
  (let* ((input-map (read-map-from-file "../tests/test-input3"))
         (part-map (find-numbers-and-parts input-map))
         (part-numbers (find-part-numbers part-map input-map)))
    (is equal (sort part-numbers #'<) (list 3 4 58 592 598 617 633 664 755))))

(define-test find-gear-ratio-finds-the-gear-ratio-of-a-part-at-a-coordinate
  :parent day3-suite
  (let* ((input-map (read-map-from-file "../tests/test-input3"))
         (part-map (find-numbers-and-parts input-map)))
    (is equal (find-gear-ratio (complex 9 4) part-map input-map) 397072)))

(define-test all-gear-ratios-finds-all-gear-ratios 
  :parent day3-suite
  (let* ((input-map (read-map-from-file "../tests/test-input3"))
         (part-map (find-numbers-and-parts input-map)))
    (is equal (all-gear-ratios part-map input-map) 
        (list 446960 397072))))
