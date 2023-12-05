(defpackage day5-tests
  (:use :cl :day5 :parachute :pears))

(in-package :day5-tests)

(define-test day5-suite)

(define-test find-mapped-number-finds-the-destination-for-a-number 
  :parent day5-suite
  (let ((mapping (parse-string (parse-map) "seed-to-soil map:
50 98 2
52 50 48")))
    (is = (find-mapped-number 53 mapping) 55)
    (is = (find-mapped-number 99 mapping) 51)))

(define-test find-location-for-seed-finds-the-location-for-a-seed
  :parent day5-suite
  (let ((almanac (parse-almanac-from-file "../tests/test-input5")))
    (is = (find-location-for-seed 79 almanac) 82)
    (is = (find-location-for-seed 14 almanac) 43)
    (is = (find-location-for-seed 55 almanac) 86)
    (is = (find-location-for-seed 13 almanac) 35)))

(define-test intersect-range-and-range-mapping-maps-overlap-and-creates-new-ranges
  :parent day5-suite
  (let ((range-mapping (parse-string (parse-range-map) "50 98 2"))
        (range (make-range :start 90 :end 100)))
    (is = 3 (length (intersect-range-and-range-mapping range range-mapping)))))

(define-test map-range-maps-all-parts-of-a-range 
  :parent day5-suite
  (let ((mapping (parse-string (parse-map) "seed-to-soil map:
50 98 2
52 50 48"))
        (range (make-range :start 49 :end 100)))
    (is equalp
        (list (make-range :start 50 :end 51) 
              (make-range :start 100 :end 100)
              (make-range :start 52 :end 99)
              (make-range :start 49 :end 49))
        (map-range range (mapping-mappings mapping)))))

(define-test map-ranges-to-locations-maps-all-ranges-to-all-locations
  :parent day5-suite
  (let ((almanac (parse-almanac-from-file "../tests/test-input5")))
    (is equalp (list (make-range :start 60 :end 60) (make-range :start 46 :end 55)
                     (make-range :start 82 :end 84) (make-range :start 68 :end 68)
                     (make-range :start 86 :end 89) (make-range :start 94 :end 96)
                     (make-range :start 56 :end 59) (make-range :start 97 :end 99))
        (map-ranges-to-locations (seed-numbers-to-ranges 
                                  (seeds-numbers (almanac-seeds almanac))) almanac))))
