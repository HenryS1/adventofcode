(defpackage day17-tests
  (:use :cl :day17 :parachute :pears))

(in-package :day17-tests)

(define-test day17-suite)

(define-test navigate-finds-the-least-heat-loss-to-get-to-the-destination
  :parent day17-suite
  (let ((the-map (read-map-from-file "../tests/test-input17")))
    (is = 102 (navigate the-map))))

(define-test navigate-ultra-finds-the-least-heat-loss-for-an-ultra-crucible
  :parent day17-suite
  (let ((the-map (read-map-from-file "../tests/test-input17")))
    (is = 94 (navigate-ultra the-map))))
