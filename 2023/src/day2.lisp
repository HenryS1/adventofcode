(defpackage :day2
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind)
  (:export 
   :parse-colour
   :parse-cube
   :parse-round
   :parse-rounds
   :parse-game
   :parse-games
   :game-rounds
   :game-id
   :is-subset
   :green
   :blue
   :red
   :game-is-possible
   :find-possible-games
   :fewest-required-cubes
   :fewest-required-cubes-for-colour
   :power))

(in-package :day2)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct game id rounds)

(defun parse-colour ()
  (fmap #l(intern (string-upcase %name) :day2) 
        (orp (seq "blue")
             (seq "red")
             (seq "green"))))

(defun parse-cube ()
  (sequential (n *non-negative-int*)
              (_ (char1 #\space))
              (colour (parse-colour))
              (cons colour n)))

(defun parse-round ()
  (fmap 
   #l(loop with round = (make-hash-table :test 'equal)
           for (colour . n) in %cubes
           do (setf (gethash colour round) 
                    (+ (gethash colour round 0) n))
           finally (return round))
   (sep-by 
    (parse-cube)
    (seq ", "))))

(defun parse-rounds ()
  (sep-by (parse-round)
          (seq "; ")))

(defun parse-game ()
  (sequential (_ (seq "Game "))
              (id *non-negative-int*)
              (_ (seq ": "))
              (rounds (parse-rounds))
              (make-game :id id :rounds rounds)))

(defun parse-games ()
  (lines (parse-game)))

(defun read-games ()
  (parse-file "input2" (parse-games)))

(defun is-subset (round bag)
  (loop for colour being the hash-keys of round using (hash-value n)
        for bag-count = (gethash colour bag 0)
        when (> n bag-count)
          do (return nil)
        finally (return t)))

(defun game-is-possible (game bag)
  (every #l(is-subset %round bag) (game-rounds game)))

(defun find-possible-games (games bag)
  (remove-if-not #l(game-is-possible %game bag) games))

(defparameter *part1-bag* (parse-string (parse-round) "12 red, 13 green, 14 blue"))

(defun part1 ()
  (reduce #'+ (mapcar #'game-id (find-possible-games (read-games) *part1-bag*))))

(defun fewest-required-cubes-for-colour (game colour)
  (loop for round in (game-rounds game)
        maximizing (gethash colour round 0)))

(defun fewest-required-cubes (game)
  (mapcar (lambda (colour) (fewest-required-cubes-for-colour game colour)) 
          (list 'blue 'red 'green)))

(defun power (game)
  (reduce #'* (fewest-required-cubes game)))

(defun sum-of-powers (games)
  (reduce #'+ (mapcar #'power games)))

(defun part2 ()
  (sum-of-powers (read-games)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
