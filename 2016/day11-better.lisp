(eval-when (:compile-toplevel :load-toplevel)
  (load "../2018/priority-queue.lisp")
  (ql:quickload :iterate)
  (ql:quickload :alexandria)
  (ql:quickload :metabang-bind))

(defpackage :day11
  (:use :cl :iterate :alexandria :bind))

(in-package :day11)

(defun safe-floor (ms gs)
  (or (= (logand ms gs) ms)
      (= gs 0)))

(defun get-floor (floors n num-ms)
  (logand (ash floors (- (* (max 0 n) (* num-ms 2)))) 
          (- (ash 1 (* 2 num-ms)) 1)))

(defun ms-and-gs (floor num-ms)
  (values (logand (ash floor (- num-ms)) (- (ash 1 num-ms) 1))
          (logand floor (- (ash 1 num-ms) 1))))

(defun count-bits (i)
  (iter (while (> i 0))
        (count (oddp i))
        (setf i (ash i -1))))

(defun priority (moves-so-far floors num-ms)
  (+ moves-so-far 
     (iter (for i from 0 to 3)
           (for equipment = (get-floor floors i num-ms))
           (summing (+ (* (- 3 i) 2 (- (count-bits equipment) 2)) (- 3 i))))))

(defun finished (num-ms)
  (lambda (floors)
    (= (get-floor floors 3 num-ms) (- (ash 1 num-ms) 1))))

(defun toggle-m (floors m elevator num-ms)
  (logxor floors (ash 1 (+ m (* elevator 2 num-ms) num-ms))))

(defun toggle-g (floors g elevator num-ms)
  (logxor floors (ash 1 (+ g (* elevator 2 num-ms)))))

(defun write-binary (n) (write-to-string n :base 2))

(defun enqueue-next-states (seen moves-so-far elevator floors direction num-ms pq)
  ;; (format t "MOVES SO FAR ~a~%" moves-so-far)
  ;; (format t "ELEVATOR ~a~%" elevator)
  ;; (format t "DIRECTION ~a~%" direction)
  (bind (((:values ms gs) (ms-and-gs (get-floor floors elevator num-ms) num-ms))
         ((:values mso gso) (ms-and-gs (get-floor floors (+ elevator direction) num-ms) num-ms)))
    (assert (safe-floor ms gs))
    (assert (safe-floor mso gso))
    (format t "MS ~a~%GS ~a~%MSO ~a~%GSO ~a~%" (write-binary ms) (write-binary gs) 
            (write-binary mso) (write-binary gso))
     (iter (for i from 0 to (- num-ms 1))
           (when (and 
                  (or (= gso 0)
                      (> (logand (ash 1 i) gso) 0))
                  (> (logand (ash 1 i) ms) 0))
             (let ((new-state floors))
               (setf new-state (toggle-m new-state i elevator num-ms))
               (setf new-state (toggle-m new-state i (+ elevator direction) num-ms))
               ;; (format t "INSERT STATE -1 ~a~%" new-state)
               (format t "I ~a ELEVATOR ~a DIRECTION ~a MOVES ~a OLD-STATE 0 ~a~%" i 
                       elevator
                       direction
                       moves-so-far
                       (write-to-string floors :base 2))
               (format t "I ~a ELEVATOR ~a DIRECTION ~a MOVES ~a NEW-STATE 0 ~a~%" 
                       i
                       elevator
                       direction
                       moves-so-far 
                       (write-to-string new-state :base 2))
               (bind (((:values ms gs) (ms-and-gs (get-floor new-state elevator num-ms) num-ms))
                      ((:values mso gso)
                       (ms-and-gs (get-floor new-state (+ elevator direction) num-ms) num-ms)))
                 (assert (safe-floor ms gs))
                 (assert (safe-floor mso gso)))
               (when (not (gethash new-state seen))
                 (setf (gethash new-state seen) t)
                 (insert-pq (list (priority moves-so-far new-state num-ms) (+ moves-so-far 1)
                                  (+ elevator direction) new-state) pq)))))
     (iter (for i from 0 to (- num-ms 1))
           (when (and 
                  (or (= mso gso)
                      (= mso 0))
                  (> (logand (ash 1 i) gs) 0)
                  (or (= (logand (ash 1 i) ms) 0)
                      (= gs (ash 1 i))))
             (let ((new-state floors))
               (setf new-state (toggle-g new-state i elevator num-ms))
               (setf new-state (toggle-g new-state i (+ elevator direction) num-ms))
               ;; (format t "INSERT STATE 0~%")
               (format t "I ~a ELEVATOR ~a DIRECTION ~a MOVES ~a OLD-STATE 1 ~a~%" i 
                       elevator
                       direction
                       moves-so-far
                       (write-to-string floors :base 2))
               (format t "I ~a ELEVATOR ~a DIRECTION ~a MOVES ~a NEW-STATE 1 ~a~%" 
                       i
                       elevator
                       direction
                       moves-so-far 
                       (write-to-string new-state :base 2))
               (bind (((:values ms gs) (ms-and-gs (get-floor new-state elevator num-ms) num-ms))
                      ((:values mso gso)
                       (ms-and-gs (get-floor new-state (+ elevator direction) num-ms) num-ms)))
                 (assert (safe-floor ms gs))
                 (assert (safe-floor mso gso)))
               (when (not (gethash new-state seen))
                 (setf (gethash new-state seen) t)
                 (insert-pq (list (priority moves-so-far new-state num-ms) (+ moves-so-far 1) 
                                  (+ elevator direction) new-state) pq)))))
     (iter (for i from 0 to (- num-ms 1))
           (when (and 
                  (> (logand (ash 1 i) ms) 0)
                  (> (logand (ash 1 i) gs) 0)
                  (or (= gso mso)
                      (= mso 0)))
             (let ((new-state floors))
               (setf new-state (toggle-m new-state i elevator num-ms))
               (setf new-state (toggle-m new-state i (+ elevator direction) num-ms))
               (setf new-state (toggle-g new-state i elevator num-ms))
               (setf new-state (toggle-g new-state i (+ elevator direction) num-ms))
               ;; (format t "INSERT STATE~%")
               (format t "I ~a ELEVATOR ~a DIRECTION ~a MOVES ~a OLD-STATE 2 ~a~%" i 
                       elevator
                       direction
                       moves-so-far
                       (write-to-string floors :base 2))
               (format t "I ~a ELEVATOR ~a DIRECTION ~a MOVES ~a NEW-STATE 2 ~a~%" 
                       i
                       elevator
                       direction
                       moves-so-far 
                       (write-to-string new-state :base 2))
               (bind (((:values ms gs) (ms-and-gs (get-floor new-state elevator num-ms) num-ms))
                      ((:values mso gso)
                       (ms-and-gs (get-floor new-state (+ elevator direction) num-ms) num-ms)))
                 (assert (safe-floor ms gs))
                 (assert (safe-floor mso gso)))
               (when (not (gethash new-state seen))
                 (insert-pq (list (priority moves-so-far new-state num-ms) (+ moves-so-far 1)
                                  (+ elevator direction) new-state) pq)))))))

(defparameter *floors-part-1* #b0000000000111100000000000111100000100001)

(defun comp-states (one other)
  (< (car one) (car other)))

(defun find-path (floors)
  (let* ((pq (make-pq #'comp-states))
         (target (count-bits floors))
         (finishedp (finished target))
         (num-ms (floor target 2))
         (seen (make-hash-table)))
    (format t "TARGET ~a NUM-MS ~a~%" target num-ms)
    (insert-pq (list (priority 0 floors num-ms) 0 0 floors) pq)
    (iter (format  t "QP SIZE ~a~%" (pq-size pq))
          (for (priority moves-so-far elevator state) = (pop-pq pq))
          (while (not (funcall finishedp state)))
          (iter (for direction in '(1 -1))
                (when (<= 0 (+ elevator direction) 3)
                  (enqueue-next-states seen moves-so-far elevator state direction num-ms pq)))
          (finally (return (cons moves-so-far state))))))
