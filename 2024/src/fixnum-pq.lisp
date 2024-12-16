(defpackage :fixnum-pq
  (:use :cl)
  (:export 
   :make-pq
   :pq-empty
   :pq-nonempty
   :pq-size
   :insert-pq
   :pop-pq))

(in-package :fixnum-pq)

(defstruct fixnum-pq (elements (make-array 0 :element-type 'fixnum) :type (simple-array fixnum))
           (size 0 :type fixnum) (fill-pointer 0 :type fixnum))

(defun make-pq (size &rest contents)
  (let ((pq (make-fixnum-pq :elements (make-array (+ size 1) :element-type 'fixnum) :size size :fill-pointer 1)))
    (loop for e in contents
       do (insert-pq e pq))
    pq))

(declaim (inline pq-nonempty))
(defun pq-nonempty (pq)
  (> (fixnum-pq-fill-pointer pq) 1))

(declaim (inline pq-empty))
(defun pq-empty (pq)
  (<= (fixnum-pq-fill-pointer pq) 1))

(declaim (inline pq-size))
(defun pq-size (pq)
  (fixnum-pq-size pq))

(declaim (inline bubble-up))
(defun bubble-up (q index)
  (declare (optimize (speed 3)) (fixnum index) ((simple-array fixnum) q))
  (loop while (and (> index 1)
                   (< (aref q index)
                      (aref q (floor index 2))))
     do (rotatef (aref q index) (aref q (floor index 2)))
        (setf index (floor index 2))))

(declaim (inline insert-pq))
(defun insert-pq (el pq)
  (let ((q (fixnum-pq-elements pq)))
    (setf (aref q (fixnum-pq-fill-pointer pq)) el)
    (incf (fixnum-pq-fill-pointer pq))
    (bubble-up q (- (fixnum-pq-fill-pointer pq) 1))))

(defun validate (q index size)
  (if (> index size)
      t
      (and (if (< (* index 2) size)
               (progn 
                 (assert (not (< (aref q (* index 2)) (aref q index))))
                 (not (< (aref q (* index 2)) (aref q index))))
               t)
           (if (< (1+ (* index 2)) size)
               (progn 
                 (assert (not (< (aref q (1+ (* index 2))) (aref q index))))
                 (not (< (aref q (1+ (* index 2))) (aref q index))))
               t)
           (validate q (* 2 index) size)
           (validate q (1+ (* 2 index)) size))))

(declaim (inline sink))
(defun sink (q size)
  (declare (optimize (speed 3)) (fixnum size) ((simple-array fixnum) q))
  (loop with index fixnum = 1
        with el fixnum = (aref q index)
        with child fixnum = index
        do (when (and (<= (* 2 index) size)
                      (< (aref q (* 2 index)) el))
             (setf child (* 2 index)))
           (when (and (<= (1+ (* 2 index)) size)
                      (< (aref q (1+ (* 2 index))) (aref q child)))
             (setf child (1+ (* 2 index))))
        while (/= child index)
        do (rotatef (aref q index) (aref q child))
           (setf index child)))

(declaim (inline pop-pq))
(defun pop-pq (pq)
  (let ((q (fixnum-pq-elements pq)) 
        (size (- (fixnum-pq-fill-pointer pq) 1)))
    (declare (optimize (speed 3)) (fixnum size) ((simple-array fixnum) q))
    (if (= size 0)
        nil
        (let ((top (aref q 1)))
          (rotatef (aref q 1) (aref q size))
          (decf (fixnum-pq-fill-pointer pq))
          (sink q (- size 1))
          top))))
