(defpackage day2-tests
  (:use :cl :day2 :parachute :pears))

(in-package :day2-tests)

(define-test day2-suite)

(define-test parse-colour-parses-colur-name-to-symbol
  :parent day2-suite
  (is equal (parse-string (parse-colour) "blue") 'blue)
  (is equal (parse-string (parse-colour) "green") 'green)
  (is equal (parse-string (parse-colour) "red") 'red))

(define-test parse-round-makes-a-hash-table-with-counts-per-colour
  :parent day2-suite
  (is equal
      (alexandria:hash-table-alist (parse-string (parse-round) "3 blue, 4 red"))
      '((red . 4) (blue . 3))))

(define-test parse-game-parses-the-game-id-and-multiple-rounds
  :parent day2-suite
  (let ((game (parse-string (parse-game)
                            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")))
    (is = (game-id game) 1)
    (is equal (mapcar #'alexandria:hash-table-alist (game-rounds game))
        '(((red . 4) (blue . 3)) ((blue . 6) (green . 2) (red . 1)) ((green . 2))))))

(define-test parse-games-parses-multiple-games
  :parent day2-suite
  (let ((games (parse-string (parse-games) "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")))
    (is equal (length games) 5)))

(defparameter *bag* (alexandria:alist-hash-table '((day2:green . 5) 
                                                   (day2:blue . 7)
                                                   (day2:red . 3))))

(define-test round-with-less-than-or-equal-of-each-than-bag-is-subset
  :parent day2-suite
  (let ((round (alexandria:alist-hash-table '((day2:green . 4) 
                                              (day2:blue . 2)
                                              (day2:red . 3)))))
    (true (is-subset round *bag*))))

(define-test round-with-more-of-any-colour-is-not-subset
  :parent day2-suite
  (let ((round1 (alexandria:alist-hash-table '((day2:green . 6) (day2:blue . 2) (day2:red . 1))))
        (round2 (alexandria:alist-hash-table '((day2:green . 1) (day2:blue . 8) (day2:red . 1))))
        (round3 (alexandria:alist-hash-table '((day2:green . 1) (day2:blue . 3) (day2:red . 4)))))
    (false (is-subset round1 *bag*))
    (false (is-subset round2 *bag*))
    (false (is-subset round3 *bag*))))

(define-test game-is-possible-if-all-rounds-are-subsets
  :parent day2-suite
  (let ((game (parse-string (parse-game)
                            "Game 1: 3 blue, 2 red; 1 red, 2 green, 6 blue; 2 green")))
    (true (game-is-possible game *bag*))))

(define-test game-is-not-possible-if-any-round-is-not-a-subset
  :parent day2-suite
  (let ((game (parse-string (parse-game)
                            "Game 1: 3 blue, 4 red; 1 red, 2 green, 8 blue; 2 green")))
    (false (game-is-possible game *bag*))))

(defparameter *bag2* (parse-string (parse-round) "12 red, 13 green, 14 blue"))

(define-test find-possible-games-keeps-all-games-which-are-possible
  (let ((games (parse-string (parse-games) "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")))
    (is equal
        (mapcar #'game-id (find-possible-games games *bag2*))
        (list 1 2 5))))

(define-test fewest-required-cubes-finds-the-fewest-required-cubes-of-each-colour
  :parent day2-suite
  (let ((game (parse-string (parse-game) 
                            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")))
    (is = (gethash 'blue (car (game-rounds game)) 0) 3)
    (is equal 
        (list 6 4 2)
        (fewest-required-cubes game))))

(define-test power-is-product-of-fewest-required-cubes
  :parent day2-suite
  (let ((game (parse-string (parse-game) 
                            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")))
    (is = (power game) 48)))
