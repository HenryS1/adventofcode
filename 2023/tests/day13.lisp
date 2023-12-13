(defpackage day13-tests
  (:use :cl :day13 :parachute :pears))

(in-package :day13-tests)

(define-test day13-suite)

(define-test find-horizontal-reflection-finds-row-where-horizontal-reflection-occurs
  :parent day13-suite
  (let ((pattern (parse-string (parse-pattern) "#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")))
    (is = 3 (find-horizontal-reflection pattern))
    (false (find-vertical-reflection pattern))))

(define-test find-horizontal-reflection-takes-into-account-smudge
  :parent day13-suite
  (let ((pattern (parse-string (parse-pattern) "#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"))
        (pattern2 (parse-string (parse-pattern) "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.")))
    (is = 0 (find-horizontal-reflection pattern :smudge 1))
    (is = 2 (find-horizontal-reflection pattern2 :smudge 1))))

(define-test find-vertical-reflection-finds-column-where-vertical-reflection-occurs
  :parent day13-suite
  (let ((pattern (parse-string (parse-pattern) "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.")))
    (is = 4 (find-vertical-reflection pattern))
    (false (find-horizontal-reflection pattern))))


(define-test find-value-of-patterns-finds-total-value-of-patterns
  :parent day13-suite
  (let ((patterns (parse-string (parse-patterns) "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")))
    (is = 405 (find-value-of-patterns patterns))))

