(defsystem "aoc2024"
  :version "1.0.0"
  :author "Henry Steere"
  :license "MIT"
  :depends-on ("cl-ppcre" 
               "iterate"
               "pears"
               "anaphora" 
               "arrow-macros"
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
                 (:file "day3")))
               (:module "tests"
                :components
                ((:file "day1"))))
  :description "Advent of Code 2024")

;; (defsystem "aoc2022/tests"
;;   :depends-on ("rove" "aoc2022")
;;   :components ((:module "tests"
;;                 :components))
;;   :perform (test-op (o c) (symbol-call :rove #':run c)))

