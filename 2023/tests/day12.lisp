(defpackage day12-tests
  (:use :cl :day12 :parachute :pears))

(in-package :day12-tests)

(define-test day12-suite)

(define-test parse-condition-record-parses-springs-and-broken-positions
  :parent day12-suite
  (let ((condition-record (parse-string (parse-condition-record) "???.### 1,1,3")))
    (is equalp
        condition-record 
        (make-condition-record :springs (coerce "???.###" 'list) :broken (list 1 1 3)))))

(define-test count-arrangements-counts-all-possible-arrangements
  :parent day12-suite
  (let ((condition-record (parse-string (parse-condition-record) "???.### 1,1,3"))
        (condition-record2 (parse-string (parse-condition-record) ".??..??...?##. 1,1,3"))
        (condition-record3 (parse-string (parse-condition-record) "?###???????? 3,2,1"))
        (condition-record4 (parse-string (parse-condition-record) "####.##.# 3,2,1"))
        (condition-record5 (parse-string (parse-condition-record) "????.######..#####. 1,6,5"))
        (condition-record6 (parse-string (parse-condition-record) "????.#...#... 4,1,1"))
        (condition-record7 (parse-string (parse-condition-record) "?#?#?#?#?#?#?#? 1,3,1,6"))
        (condition-record8 (parse-string (parse-condition-record) "?###???##??# 3,2,1")))
    (is = 1 (count-arrangements condition-record))
    (is = 4 (count-arrangements condition-record2))
    (is = 10 (count-arrangements condition-record3))
    (is = 0 (count-arrangements condition-record4))
    (is = 4 (count-arrangements condition-record5))
    (is = 1 (count-arrangements condition-record6))
    (is = 1 (count-arrangements condition-record7))
    (is = 1 (count-arrangements condition-record8))
    ))
