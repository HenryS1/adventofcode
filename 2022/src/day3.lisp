(defpackage :day3
  (:use :cl :iterate :alexandria :anaphora :metabang-bind
        :aoc.functional :pears
        :arrow-macros)
  (:shadowing-import-from #:arrow-macros #:<>))

(in-package :day3)

(currying:enable-currying-syntax)

(defun priority (e)
  (loop for i from 1
        for c across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        until (char= c e)
        finally (return i)))

(defun split-in-half (items)
  (cons (map 'list #'identity (subseq items 0 (floor (length items) 2)))
        (map 'list #'identity (subseq items (floor (length items) 2)))))

(defun parse-lines ()
  (parse-file "day3.input" (sep-by (many1 #'alpha-char-p) (char1 #\newline))))

(defun occurring-in-both (items)
  (bind (((fst . snd) (split-in-half items)))
    (remove-duplicates (intersection fst snd))))

(defun part1 ()
  (->> (mapcar #'occurring-in-both (parse-lines))
    (mapcar #p(mapcar #'priority))
    (mapcar #p(reduce #'+))
    (reduce #'+)))

(defun groups-of-3 (items)
  (parse-sequence items (repeated (fmap #l(coerce %result 'list) (anyn 3)))
                  :output-type 'list)) 

(defun find-badge (lines)
  (let ((groups (mapcar #p(map 'list #'identity) lines)))
    (->> (intersection (cadr groups) (caddr groups))
      (intersection (car groups))
      (remove-duplicates))))

(defun part2 ()
  (->> (parse-lines)
    (groups-of-3)
    (mapcar #'find-badge)
    (flatten)
    (mapcar #'priority)
    (reduce #'+)))
