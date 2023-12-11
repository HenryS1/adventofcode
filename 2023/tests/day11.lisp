(defpackage day11-tests
  (:use :cl :day11 :parachute :pears))

(in-package :day11-tests)

(define-test day11-suite)

(define-test find-empty-rows-finds-rows-without-stars
  :parent day11-suite
  (let ((galaxies (read-galaxies-from-file "../tests/test-input11")))
    (is equal '(3 7) (find-empty-rows galaxies))))

(define-test find-empty-columns-finds-columns-without-stars
  :parent day11-suite
  (let ((galaxies (read-galaxies-from-file "../tests/test-input11")))
    (is equal '(2 5 8) (find-empty-columns galaxies))))

(define-test find-star-coordinates-finds-locations-of-all-stars
  :parent day11-suite
  (let ((galaxies (read-galaxies-from-file "../tests/test-input11")))
    (is equal (reverse (list (make-coord 0 3) 
                             (make-coord 1 7)
                             (make-coord 2 0)
                             (make-coord 4 6)
                             (make-coord 5 1)
                             (make-coord 6 9)
                             (make-coord 8 7)
                             (make-coord 9 0)
                             (make-coord 9 4))) 
        (find-star-coordinates galaxies))))

(define-test adjust-star-coordinate-gives-star-coordinate-after-expanding-space
  :parent day11-suite
  (let* ((galaxies (read-galaxies-from-file "../tests/test-input11"))
         (empty-rows (find-empty-rows galaxies))
         (empty-columns (find-empty-columns galaxies)))
    (is equal 
        (make-coord 5 8)
        (adjust-star-coordinate (make-coord 4 6) empty-rows empty-columns))))

(define-test adjust-star-coordinate-gives-star-coordinate-uses-factor-to-expand-space
  :parent day11-suite
  (let* ((galaxies (read-galaxies-from-file "../tests/test-input11"))
         (empty-rows (find-empty-rows galaxies))
         (empty-columns (find-empty-columns galaxies)))
    (is equal 
        (make-coord 14 26)
        (adjust-star-coordinate (make-coord 4 6) empty-rows empty-columns :factor 10))))

