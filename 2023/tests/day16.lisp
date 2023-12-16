(defpackage day16-tests
  (:use :cl :day16 :parachute :pears))

(in-package :day16-tests)

(define-test day16-suite)

(define-test trace-light-path-finds-number-of-energized-tiles
  :parent day16-suite
  (let ((mirrors (read-mirrors-from-file "../tests/test-input16")))
    (is = 46 (trace-light-path mirrors (cons (make-coord 0 0) 'right)))))

(define-test find-maximum-lava-generation-finds-the-most-lava-generated
  :parent day16-suite
  (let ((mirrors (read-mirrors-from-file "../tests/test-input16")))
    (is = 51 (find-maximum-lava-generation mirrors))))
