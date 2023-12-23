(defpackage :day22
  (:use 
   :cl 
   :iterate 
   :anaphora 
   :alexandria
   :pears
   :metabang-bind
   :priority-queue
   :queue)
  (:export
   ))

(in-package :day22)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct brick x1 y1 z1 x2 y2 z2 id)

(defun parse-brick ()
  (sequential (x1 *non-negative-int*)
              (_ (char1 #\,))
              (y1 *non-negative-int*)
              (_ (char1 #\,))
              (z1 *non-negative-int*)
              (_ (char1 #\~))
              (x2 *non-negative-int*)
              (_ (char1 #\,))
              (y2 *non-negative-int*)
              (_ (char1 #\,))
              (z2 *non-negative-int*)
              (make-brick :x1 x1 :y1 y1 :z1 z1 :x2 x2 :y2 y2 :z2 z2)))

(defun parse-bricks ()
  (fmap #l (sort-by-z (loop for i from 0 for brick in %bricks
                            do (setf (brick-id brick) i)
                            finally (return %bricks)))
        (sep-by (parse-brick) (many1 #'newlinep))))

(defun read-bricks-from-file (filename)
  (parse-file filename (parse-bricks)))

(defun sort-by-z (bricks) 
  (sort bricks #'< :key #l(min (brick-z1 %brick) (brick-z2 %brick))))

;; (defun is-oblique (brick)
;;   (or (and (/= (brick-x1 brick) (brick-x2 brick))
;;            (/= (brick-y1 brick) (brick-y2 brick)))
;;       (and (/= (brick-x1 brick) (brick-x2 brick))
;;            (/= (brick-z1 brick) (brick-z2 brick)))
;;       (and (/= (brick-y1 brick) (brick-y2 brick))
;;            (/= (brick-z1 brick) (brick-z2 brick)))))

;; (defun overlap (one other)
;;   (or (and (<= (brick-x1 one) (brick-x1 other) (brick-x2 one))
;;            (<= (brick-y1 other) (brick-y1 one) (brick-y2 other)))
;;       (and (< (brick-x1 other) (brick-x1 one) (brick-x2 other))
;;            (overlap other one))))

;; (defun fall (bricks)
;;   (loop for remaining on bricks
;;         for below = nil then (cons brick below)
;;         for brick = (car remaining)
;;         while brick
;;         do (let ((all-overlaps (remove-if-not #p(overlap brick) below)))
;;              (format t "BELOW ~a~%" below)
;;              (if (null first-overlap)
;;                  (let ((diff (- (min (brick-z1 brick) (brick-z2 brick)) 1)))
;;                    (format t "OVERLAP ~a DIFF ~a~%" first-overlap diff)
;;                    (decf (brick-z1 brick) diff)
;;                    (decf (brick-z2 brick) diff))
;;                  (let ((diff (- (- (min (brick-z1 brick) (brick-z2 brick)) 
;;                                    (max (brick-z1 first-overlap) (brick-z2 first-overlap)))
;;                                 1)))
;;                    (format t "BRICK ~a~% OVERLAP ~a~% DIFF ~a~%" brick first-overlap diff)
;;                    (decf (brick-z1 brick) diff)
;;                    (decf (brick-z2 brick) diff)))))
;;   bricks)

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)

