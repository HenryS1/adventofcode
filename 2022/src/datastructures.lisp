(defpackage :aoc.datastructures
  (:use :cl :metabang-bind)
  (:export :make-queue :enqueue :enqueue-all :dequeue :q-empty))

(in-package :aoc.datastructures)

(defun make-queue (l)
  (cons (reverse l) nil))

(defun enqueue (e q)
  (bind (((back . front) q))
    (cons (cons e back) front)))

(defun enqueue-all (es q)
  (bind (((back . front) q))
    (cons (append (reverse es) back) front)))

(defun dequeue (q)
  (bind (((back . front) q))
    (if (null front)
        (let ((new-front (reverse back)))
          (cons (car new-front) (cons nil (cdr new-front))))
        (cons (car front) (cons back (cdr front))))))

(defun q-empty (q)
  (and (null (car q)) (null (cdr q))))

(defun empty-pq () nil)

(defun singleton-pq (e)
  (make-leftist-heap :elem e))

(defgeneric less (one other))

(defstruct (leftist-heap (:conc-name lpq-))
  (rank 1 :type (unsigned-byte 64))
  elem
  (left nil :type (or leftist-heap null))
  (right nil :type (or leftist-heap null)))

(defun pq-rank (q)
  (if (null q) 0 (lpq-rank q)))

(defun make-t (elem left right)
  (if (>= (pq-rank left) (pq-rank right))
      (make-leftist-heap :rank (+ (pq-rank right) 1)
                         :elem elem 
                         :left left 
                         :right right)
      (make-leftist-heap :rank (+ (pq-rank left) 1)
                         :elem elem
                         :left right
                         :right left)))

(defun merge-pq (one other)
  (cond ((null one) other)
        ((null other) one)
        (t (if (less (lpq-elem one) (lpq-elem other))
               (make-t (lpq-elem one) (lpq-left one)
                       (merge-pq (lpq-right one) other))
               (make-t (lpq-elem other) (lpq-left other)
                       (merge-pq one (lpq-right other)))))))

(defun insert-pq (e one)
  (merge-pq (singleton-pq e) one))

(defun find-pq-min (q)
  (when q (lpq-elem q)))

(defun remove-pq-min (q)
  (if (null q) 
      (values nil nil)
      (values (lpq-elem q) (merge-pq (lpq-left q) (lpq-right q)))))

(defmethod less ((n number) (m number)) (< n m))
(defmethod less ((a string) (b string)) (string< a b))
(defmethod less ((a symbol) (b symbol)) (string< (symbol-name a) (symbol-name b)))

(defun pq-from-list (l)
  (reduce (lambda (q e) (insert-pq e q)) l :initial-value nil))

(defun pq-to-list (q)
  (labels ((rec (remaining acc)
             (if (null remaining)
                 (reverse acc)
                 (multiple-value-bind (mn rest) (remove-pq-min remaining)
                   (rec rest (cons mn acc))))))
    (rec q nil)))

