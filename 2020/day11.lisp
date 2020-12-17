(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :trivia)
  (ql:quickload :trivia.ppcre)
  (ql:quickload :cl-ppcre)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind)
  (ql:quickload :alexandria)
  (ql:quickload :cl-arrows)
  (load "../2018/queue.lisp")
  (load "../2018/priority-queue.lisp"))

(defpackage :day11
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day11)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input11" using #'read-line)
        (collect line)))

(defun count-nbrs (r c map)
  (iter (for row from (- r 1) to (+ r 1))
        (summing (iter (for col from (- c 1) to (+ c 1))
                       (when (or (/= row r) (/= col c))
                         (summing (gethash (cons row col) map 0)))))))

(defun read-map (lines)
  (iter (with map = (make-hash-table :test 'equal))
        (for line in lines)
        (for r from 0)
        (iter (for c from 0)
              (for ch in-string line)
              (when (char= ch #\L) 
                (setf (gethash (cons r c) map) 0)))
        (finally (return map))))

(defun neighbouring-seats (r c map rows cols)
  (append (iter (for row from (+ r 1) to (- rows 1))
                (when (gethash (cons row c) map)
                  (collect (cons row c)))
                (while (not (gethash (cons row c) map))))
          (iter (for row from (- r 1) downto 0)
                (when (gethash (cons row c) map)
                  (collect (cons row c)))
                (while (not (gethash (cons row c) map))))
          (iter (for col from (+ c 1) to (- cols 1))
                (when (gethash (cons r col) map)
                  (collect (cons r col)))
                (while (not (gethash (cons r col) map))))
          (iter (for col from (- c 1) downto 0)
                (when (gethash (cons r col) map)
                  (collect (cons r col)))
                (while (not (gethash (cons r col) map))))
          (iter (for row from (+ r 1) to (- rows 1))
                (for col from (+ c 1) to (- cols 1))
                (when (gethash (cons row col) map)
                  (collect (cons row col)))
                (while (not (gethash (cons row col) map))))
          (iter (for row from (+ r 1) to (- rows 1))
                (for col from (- c 1) downto 0)
                (when (gethash (cons row col) map)
                  (collect (cons row col)))
                (while (not (gethash (cons row col) map))))
          (iter (for row from (- r 1) downto 0)
                (for col from (+ c 1) to (- cols 1))
                (when (gethash (cons row col) map)
                  (collect (cons row col)))
                (while (not (gethash (cons row col) map))))
          (iter (for row from (- r 1) downto 0)
                (for col from (- c 1) downto 0)
                (when (gethash (cons row col) map)
                  (collect (cons row col)))
                (while (not (gethash (cons row col) map))))))

(defun find-neighbouring-seats (map rows cols)
  (iter (with neighbours = (make-hash-table :test 'equal))
        (for ((r . c) _) in-hashtable map)
        (setf (gethash (cons r c) neighbours) (neighbouring-seats r c map rows cols))
        (finally (return neighbours))))

(defun count-visible-nbrs (r c map neighbours)
  (iter (for neighbour in (gethash (cons r c) neighbours))
        (summing (gethash neighbour map))))

(defun next-map-visible-neighbours (map neighbours)
  (iter (with next-map = (make-hash-table :test 'equal))
        (with changed = nil)
        (for ((r . c) v) in-hashtable map)
        (for taken = (count-visible-nbrs r c map neighbours))
        (for key = (cons r c))
        (when (= v 0)
          (if (= taken 0)
              (progn (setf changed t)
                     (setf (gethash key next-map) 1))
              (setf (gethash key next-map) 0)))
        (when (= v 1)
          (if (>= taken 5)
              (progn (setf changed t)
                     (setf (gethash key next-map) 0))
              (setf (gethash key next-map) 1)))
        (finally (return (cons next-map changed)))))

(defun visible-convergence (lines)
  (iter (with init-map = (read-map lines))
        (with rows = (length lines))
        (with cols = (length (car lines)))
        (with neighbours = (find-neighbouring-seats init-map rows cols))
        (for (map . changed) first (cons init-map t) 
             then (next-map-visible-neighbours map neighbours))
        (for turns from 0)
        (until (not changed))
        (finally (return (iter (for (_ v) in-hashtable map) (sum v))))))

(defun next-map (map)
  (iter (with next-map = (make-hash-table :test 'equal))
        (with changed = nil)
        (for ((r . c) v) in-hashtable map)
        (for taken = (count-nbrs r c map))
        (for key = (cons r c))
        (when (= v 0)
          (if (= taken 0)
              (progn (setf changed t)
                     (setf (gethash key next-map) 1))
              (setf (gethash key next-map) 0)))
        (when (= v 1)
          (if (>= taken 4)
              (progn (setf changed t)
                     (setf (gethash key next-map) 0))
              (setf (gethash key next-map) 1)))
        (finally (return (cons next-map changed)))))

(defun convergence (lines)
  (iter (for (map . changed) first (cons (read-map lines) t)
             then (next-map map))
        (for turns from 0)
        (until (not changed))
        (finally (return (iter (for (_ v) in-hashtable map) (sum v))))))