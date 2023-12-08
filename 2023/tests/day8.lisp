(defpackage day8-tests
  (:use :cl :day8 :parachute :pears))

(in-package :day8-tests)

(define-test day8-suite)

(define-test parse-node-parses-a-node-from-input
  :parent day8-suite
  (is equalp
      (make-node :id "AAA" :left "BBB" :right "CCC")
      (parse-string (parse-node) "AAA = (BBB, CCC)")))

(define-test follow-directions-counts-steps-until-reaching-the-end
  :parent day8-suite
  (let ((node-map1 (parse-string (parse-node-map) "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"))
        (node-map2 (parse-string (parse-node-map) "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")))
    (is = 2 (follow-directions node-map1))
    (is = 6 (follow-directions node-map2))))

(define-test find-starting-points-finds-all-starting-points
  :parent day8-suite
  (let ((node-map (parse-string (parse-node-map) "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")))
    (is equalp 
        (vector "11A" "22A") 
        (map 'vector #'node-id (find-starting-points node-map)))))

(define-test follow-multiple-paths-simultaneously-finds-the-steps-taken-for-all-paths-to-finish
  :parent day8-suite
  (let ((node-map (parse-string (parse-node-map) "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")))
    (is = 6 (follow-multiple-paths-simultaneously node-map))))
