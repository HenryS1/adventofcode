(defpackage day14-tests
  (:use :cl :day14 :parachute :pears))

(in-package :day14-tests)

(define-test day14-suite)

(define-test roll-rocks-north-rolls-rocks-as-far-north-as-they-can-go
  :parent day14-suite
  (let ((rocks (parse-string (parse-rocks) "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."))
        (expected-rocks (parse-string (parse-rocks) "OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....")))
    (roll-rocks-north rocks)
    (is equalp expected-rocks rocks)))

(define-test calculate-north-side-load-finds-the-total-load-on-the-north-side
  :parent day14-suite
  (let ((rocks (parse-string (parse-rocks) "OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....")))
    (is = 136 (calculate-north-side-load rocks))))

(define-test cycle-applies-a-cycle-of-rotations-to-the-rocks
  :parent day14-suite
  (let ((rocks (parse-string (parse-rocks) "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."))
        (one-cycle (parse-string (parse-rocks) ".....#....
....#...O#
...OO##...
.OO#......
.....OOO#.
.O#...O#.#
....O#....
......OOOO
#...O###..
#..OO#...."))
        (two-cycles (parse-string (parse-rocks) ".....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#..OO###..
#.OOO#...O"))
        (three-cycles (parse-string (parse-rocks) ".....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#...O###.O
#.OOO#...O")))
    (cycle-rocks rocks)
    (is equalp one-cycle rocks)
    (cycle-rocks rocks)
    (is equalp two-cycles rocks)
    (cycle-rocks rocks)
    (is equalp three-cycles rocks)))

;; ".OOO.#.O.."
;; ".O..#....#"
;; "OO..O##..O"
;; "O..#.OO..."
;; "........#."
;; "..#....#.#"
;; ".....#.O.."
;; "..O......."
;; "#....###.."
;; "#....#...."

;; "OOOO.#.O.." 
;; "OO..#....#"
;; "OO..O##..O"
;; "O..#.OO..."
;; "........#."
;; "..#....#.#"
;; "..O..#.O.O"
;; "..O......." 
;; "#....###.." 
;; "#....#...."
