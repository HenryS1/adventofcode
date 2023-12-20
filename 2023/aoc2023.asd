(defsystem "aoc2023"
  :version "1.0.0"
  :author "Henry Steere"
  :license "MIT"
  :depends-on ("cl-ppcre" 
               "iterate"
               "pears"
               "anaphora" 
               "arrow-macros"
               "fset"
               "ironclad"
               "flexi-streams"
               "metabang-bind"
               "alexandria"
               "trivia"
               "neat-lambda"
               "parachute"
               "trivia.ppcre")
  :components ((:module "src"
                :components
                ((:file "queue")
                 (:file "priority-queue")
                 (:file "day1")
                 (:file "day2")
                 (:file "day3")
                 (:file "day4")
                 (:file "day5")
                 (:file "day6")
                 (:file "day7")
                 (:file "day8")
                 (:file "day9")
                 (:file "day10")
                 (:file "day11")
                 (:file "day12")
                 (:file "day13")
                 (:file "day14")
                 (:file "day15")
                 (:file "day16")
                 (:file "day17")
                 (:file "day18")
                 (:file "day19")
                 (:file "day20")))
               (:module "tests"
                :components
                ((:file "day1")
                 (:file "day2")
                 (:file "day3")
                 (:file "day4")
                 (:file "day5")
                 (:file "day6")
                 (:file "day7")
                 (:file "day8")
                 (:file "day9")
                 (:file "day10")
                 (:file "day11")
                 (:file "day12")
                 (:file "day13")
                 (:file "day14")
                 (:file "day15")
                 (:file "day16")
                 (:file "day17")
                 (:file "day18")
                 (:file "day19")
                 (:file "day20"))))
  :description "Advent of Code 2023")

;; (defsystem "aoc2022/tests"
;;   :depends-on ("rove" "aoc2022")
;;   :components ((:module "tests"
;;                 :components))
;;   :perform (test-op (o c) (symbol-call :rove #':run c)))

