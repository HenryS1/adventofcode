(defpackage :day8
  (:use :cl :cl-ppcre :trivia trivia.ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day8)

(defun read-lines ()
  (map 'vector #'identity
       (iter (for line in-file "day8.input" using #'read-line)
         (collect (map 'vector #'digit-char-p line)))))

(defmacro def-visible (name outer inner)
  (with-gensyms (heights visible highest current)
    `(defun ,name (,heights)
       (let ((rows (length ,heights))
             (cols (length (aref ,heights 0)))
             (,visible (make-array (list (length ,heights) (length (aref ,heights 0)))
                                  :element-type 'bit :initial-element 0)))
         (iter ,outer
           (iter ,inner
             (with ,highest = -1)
             (for ,current = (aref (aref ,heights r) c))
             (when (< ,highest ,current) (setf (aref ,visible r c) 1))
             (setf ,highest (max ,highest ,current))))
         ,visible))))

(def-visible visible-from-top 
  (for c from 0 to (- cols 1))
  (for r from 0 to (- rows 1)))

(def-visible visible-from-bottom
  (for c from 0 to (- cols 1))
  (for r from (- rows 1) downto 0))

(def-visible visible-from-left
  (for r from 0 to (- rows 1))
  (for c from 0 to (- cols 1)))

(def-visible visible-from-right
  (for r from 0 to (- rows 1))
  (for c from (- cols 1) downto 0))

(defun visible (heights)
  (let ((rows (length heights))
        (cols (length (aref heights 0)))
        (from-left (visible-from-left heights))
        (from-right (visible-from-right heights))
        (from-top (visible-from-top heights))
        (from-bottom (visible-from-bottom heights)))
    (iter (for r from 0 to (- rows 1))
      (with count = 0)
      (iter (for c from 0 to (- cols 1))
        (when (or (= (aref from-left r c) 1)
                  (= (aref from-right r c) 1)
                  (= (aref from-top r c) 1)
                  (= (aref from-bottom r c) 1))
          (incf count)))
      (finally (return count)))))

(defun visible-count-up (heights r c)
  (let ((height (aref (aref heights r) c)))
    (labels ((rec (row count)
               (cond ((< row 0) count)
                     ((> height (aref (aref heights row) c))
                      (rec (- row 1) (+ count 1)))
                     (t (+ count 1)))))
      (rec (- r 1) 0))))

(defun visible-count-down (heights r c)
  (let ((rows (length heights))
        (height (aref (aref heights r) c)))
    (labels ((rec (row count)
               (cond ((>= row rows) count)
                     ((> height (aref (aref heights row) c))
                      (rec (+ row 1) (+ count 1)))
                     (t (+ count 1)))))
      (rec (+ r 1) 0))))

(defun visible-count-right (heights r c)
  (let ((cols (length (aref heights 0)))
        (height (aref (aref heights r) c)))
    (labels ((rec (col count)
               (cond ((>= col cols) count)
                     ((> height (aref (aref heights r) col))
                      (rec (+ col 1) (+ count 1)))
                     (t (+ count 1)))))
      (rec (+ c 1) 0))))

(defun visible-count-left (heights r c)
  (let ((height (aref (aref heights r) c)))
    (labels ((rec (col count)
               (cond ((< col 0) count)
                     ((> height (aref (aref heights r) col))
                      (rec (- col 1) (+ count 1)))
                     (t (+ count 1)))))
      (rec (- c 1) 0))))

(defun scenic-score (heights r c)
  (* (visible-count-left heights r c)
     (visible-count-right heights r c)
     (visible-count-up heights r c)
     (visible-count-down heights r c)))

(defun best-scenic-score (heights)
  (let ((rows (length heights))
        (cols (length (aref heights 0)))
        (scenic-score 0))
    (iter (for r from 0 to (- rows 1))
      (iter (for c from 0 to (- cols 1))
        (setf scenic-score (max scenic-score (scenic-score heights r c)))))
    scenic-score))
