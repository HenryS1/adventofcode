(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day22
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day22)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun new-stack (cards) 
  (reverse cards))

(defun cut (cards n)
  (if (> n 0)
      (concatenate 'vector (subseq cards n) (subseq cards 0 n))
      (cut cards (+ (length cards) n))))

(defun deal-with-increment (cards n)
  (iter (with new-deck = (make-array (length cards) :initial-element -1))
        (with remaining = (length cards))
        (with next-pointer = 0)
        (while (> remaining 0))
        (for i first 0 then (mod (+ i n) (length cards)))
        (when (= (aref new-deck i) -1)
          (setf (aref new-deck i) (aref cards next-pointer))
          (incf next-pointer)
          (decf remaining))
        (finally (return new-deck))))

(defun read-lines ()
  (iter (for line in-file "input22" using #'read-line)
        (collect line)))

(defparameter *size* 7)

(defun interpret-operations ()
  (iter (with cards = (coerce (iter (for i from 0 to (- *size* 1)) (collect i)) 'vector))
        (for line in (read-lines))
        (for is = (ints line))
        (cond ((search "increment" line)
               (setf cards (deal-with-increment cards (car is))))
              ((search "cut" line)
               (setf cards (cut cards (car is))))
              ((search "stack" line)
               (setf cards (new-stack cards))))
        (finally (return cards))))

(defun answer-1 () (position 2019 (interpret-operations)))

(defclass deck ()
  ((stride :accessor stride :initform 1)
   (direction :accessor direction :initform 1)
   (start :accessor start :initform 0)
   (size :accessor size :initarg :size)))

(defun deal-new-stack (deck)
  (setf (direction deck) (* (direction deck) -1)))

(defun cut-deck (deck n)
  (if (> n 0)
      (progn (setf n (* n (direction deck)))
             (setf (start deck) (mod (+ (start deck) n) (size deck))))
      (cut-deck deck (+ (size deck) n))))

(defun deal-deck-with-increment (deck n)
  (setf (stride deck) (mod (* (stride deck) (- (size deck) n)) (size deck))))

(defun collect-elements (deck)
  (iter (with start = (if (> (direction deck) 0) (start deck) 
                          (mod (+ (start deck)
                                  (* (stride deck) (- (size deck) 1))) (size deck))))
        (with seen = 0)
;        (format t "START ~A~%" start)
        (for ind first start then (mod (+ ind (* (stride deck) (direction deck))) (size deck)))
        (while (< seen (size deck)))
;        (format t "IND ~a~%" ind)
        (collect (mod ind (size deck)))
        (incf seen)))

(defun interpret-with-deck ()
  (iter (with deck = (make-instance 'deck :size *size*))
        (for line in (read-lines))
        (for is = (ints line))
        (cond ((search "increment" line)
               (deal-deck-with-increment deck (car is)))
              ((search "cut" line)
               (cut-deck deck (car is)))
              ((search "stack" line)
               (deal-new-stack deck)))
;        (format t "~a~%" line)
;        (format t "STRIDE ~a DIRECTION ~a START ~a~%" (stride deck) (direction deck) (start deck))
        (finally (return (collect-elements deck)))))

(defun prepare-action (line)
  (let ((is (ints line)))
    (cond ((search "increment" line)
           `(setf i (* i ,(car is))))
          ((search "cut" line)
           `(setf i (+ i ,(* (car is) -1))))
          ((search "stack" line)
           `(setf i (- size i 1))))))

(defun read-function ()
  (let ((body (mapcar #'prepare-action (read-lines))))
    (eval `(defun adjust-index (i size)
             (progn
               ,@body
               i)))))

(read-function)

(defun find-cycle ()
  (iter (with seen-diff = (make-hash-table :test 'equal))
        (for ind first 2020 then (adjust-index ind 119315717514047))
        (for ind-p previous ind)
        (for i from 1 to 100)
        (format t "IND ~a~%" ind)
        ;; (when ind-p
        ;;   (format t "RATIO ~a DIFF ~a~%" (/ ind ind-p) (- ind ind-p)))
        (when (and ind-p (gethash (- ind ind-p) seen-diff))
          (error "YAY!"))))

(defun extended-euclid (a b)
  (iter (for (old-r r) first (list a b) then (list r (- old-r (* quotient r))))
        (while (/= r 0))
        (for quotient = (floor old-r r))
        (for (old-s s) first (list 1 0) then (list s (- old-s (* quotient s))))
        (for (old-te te) first (list 0 1) then (list te (- old-te (* quotient te))))
        (finally (return old-te))))

;; (adjust-index #C(0 1) 119315717514047)
;; #C(6755816444126017704996403974577411786740314542833769357326975224574943580739621 -667487277673079497859196426642502502538645344354304000000000000000000)

;; (mod (* (- 60269698145644) 96123531687058) 119315717514047)
;; 106352671593160

;; a 96123531687058
;; b 106352671593160

(defun square-application (c)
  (+ (* (imagpart c) c) (realpart c)))

(defun repeat-square (c n)
  (iter (for i from 1 to n)
        (for result first c then (square-application result))
        (finally (return result))))
