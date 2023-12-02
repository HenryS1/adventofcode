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
               "metabang-bind"
               "alexandria"
               "trivia"
               "neat-lambda"
               "parachute"
               "trivia.ppcre")
  :components ((:module "src"
                :components
                ((:file "day1")
                 (:file "day2")))
               (:module "tests"
                :components
                ((:file "day1")
                 (:file "day2"))))
  :description "Advent of Code 2023")

;; (defsystem "aoc2022/tests"
;;   :depends-on ("rove" "aoc2022")
;;   :components ((:module "tests"
;;                 :components))
;;   :perform (test-op (o c) (symbol-call :rove #':run c)))

