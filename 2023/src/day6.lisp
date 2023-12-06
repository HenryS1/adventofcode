(defpackage :day6
  (:use 
   :cl 
   :iterate 
   :bind
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind)
  (:export
   :make-race
   :distances-for-race
   :number-of-ways-to-beat-record
   :margin-of-error
   :parse-races-from-file
   :find-first-time-record-is-broken
   :find-last-time-record-is-broken))

(in-package :day6)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun ignore-whitespace () (many #p(char= #\space)))

(defun parse-numbers ()
  (sep-by *non-negative-int* (many1 #p(char= #\space))))

(defun parse-times ()
  (sequential (_ (seq "Time:"))
              (_ (ignore-whitespace))
              (times (parse-numbers))
              times))

(defun parse-distances ()
  (sequential (_ (seq "Distance:"))
              (_ (ignore-whitespace))
              (distances (parse-numbers))
              distances))

(defstruct race time distance)

(defun parse-races ()
  (sequential (times (parse-times))
              (_ (many1 #'newlinep))
              (distances (parse-distances))
              (loop for time in times for distance in distances 
                    collect (make-race :time time :distance distance))))

(defun distances-for-race (race)
  (loop for i from 0 to (race-time race)
        collect (* i (- (race-time race) i))))

(defun number-of-ways-to-beat-record (race)
  (count-if #l(> %distance (race-distance race)) 
            (distances-for-race race)))

(defun parse-races-from-file (filename)
  (parse-file filename (parse-races)))

(defun margin-of-error (races)
  (reduce #'* (mapcar #'number-of-ways-to-beat-record races)))

(defun part1 ()
  (margin-of-error (parse-races-from-file "input6")))

(defun distance-travelled (time total-time)
  (* time (- total-time time)))

(defun find-first-time-record-is-broken (total-time record)
  (labels ((rec (lower upper)
             (let* ((mid (floor (+ lower upper) 2))
                    (mid-distance (distance-travelled mid total-time))
                    (previous-distance (distance-travelled (- mid 1) total-time)))
               (cond ((< mid-distance record)
                      (rec (+ mid 1) upper))
                     ((>= mid-distance record)
                      (if (< previous-distance record)
                          mid
                          (rec lower mid)))))))
    (rec 0 total-time)))

(defun find-last-time-record-is-broken (total-time record)
  (labels ((rec (lower upper)
             (let* ((mid (floor (+ lower upper) 2))
                    (mid-distance (distance-travelled mid total-time))
                    (previous-distance (distance-travelled (- mid 1) total-time)))
               (cond ((> mid-distance record)
                      (rec (+ mid 1) upper))
                     ((<= mid-distance record)
                      (if (> previous-distance record)
                          (- mid 1)
                          (rec lower mid)))))))
    (rec 0 total-time)))

(defun part2 ()
  (+ (- (find-last-time-record-is-broken 41968894 214178911271055)
        (find-first-time-record-is-broken 41968894 214178911271055)) 1))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
