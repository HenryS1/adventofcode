(defsystem "aoc2022"
  :version "1.0.0"
  :author "Henry Steere"
  :license "MIT"
  :depends-on ("cl-ppcre" 
               "iterate"
               "pears"
               "anaphora" 
               "arrow-macros"
               "fset"
               "metabang-bind"
               "alexandria"
               "trivia"
               "neat-lambda"
               "trivia.ppcre")
  :components ((:module "src"
                :components
                ((:file "datastructures")
                 (:file "functional")
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
                 (:file "day16"))))
  :description "Advent of Code 2022"
  :in-order-to ((test-op (test-op "aoc2022/tests"))))

;; (defsystem "aoc2022/tests"
;;   :depends-on ("rove" "aoc2022")
;;   :components ((:module "tests"
;;                 :components))
;;   :perform (test-op (o c) (symbol-call :rove #':run c)))

