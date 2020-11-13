(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :iterate)
  (ql:quickload :cl-arrows)
  (load "../2018/queue.lisp"))

(defpackage :day11
  (:use :cl :iterate :cl-arrows))

(in-package :day11)

(defun make-generator-mask (num-chips)
  (iter (with res = 0)
        (for i from 0 to (- num-chips 1))
        (setf res (+ (ash res 2) 1))
        (finally (return res))))

(defun make-chip-mask (num-chips)
  (iter (with res = 0)
        (for i from 0 to (- num-chips 1))
        (setf res (+ (ash res 2) 2))
        (finally (return res))))

(defun safe-floor (floor generator-mask chip-mask)
  (or (= (logand chip-mask floor) 0)
      (= (logand generator-mask floor) 0)
      (= (logxor (logand (ash floor -1) generator-mask) (logand floor generator-mask)) 0)))

(defun get-elevator (state)
  (ash state -60))

(defun get-floor (state floor-num num-chips)
  (ash (logand (- (ash 1 (* (+ floor-num 1) num-chips 2)) 1) 
               state)
       (- (* floor-num num-chips 2))))

(defun set-floor (state floor-num num-chips new-floor)
  (logior (logxor state (ash (get-floor state floor-num num-chips) (* floor-num num-chips 2))) 
          (ash new-floor (* floor-num num-chips 2))))

(declaim (inline subtract-rightmost-bit rightmost-bit))
(defun rightmost-bit (n) (logand n (- n)))
(defun subtract-rightmost-bit (n) (- n (rightmost-bit n)))

(defun choose-one-or-two (flr callback)
  (iter outer
        (for initial first flr then (subtract-rightmost-bit initial))
        (while (> initial 0))
        (iter (for rest first (subtract-rightmost-bit initial)
                   then (subtract-rightmost-bit rest))
              (funcall callback (logior (rightmost-bit initial) (rightmost-bit rest)))
              (until (= rest 0)))))

(defun set-elevator (current-elevator next-elevator state)
  (logxor (ash next-elevator 60) (logxor (ash current-elevator 60) state)))

(defun make-state-masks (num-chips num-floors)
  (let ((arr (make-array num-chips :element-type 'fixnum)))
    (declare ((simple-array fixnum) arr))
    (iter (for i from 0 to (- num-chips 1))
          (setf (aref arr i) 
                (iter (with initial = (ash 3 (* i 2)))
                      (for j from 0 to (- num-floors 1))
                      (for res first initial then (logior res (ash initial (* j num-chips 2))))
                      (finally (return res)))))
    arr))

(defun state-hash (st masks)
  (labels ((swap (i j)
             (let ((one (logand st (aref masks i)))
                   (other (logand st (aref masks j))))
               (setf st (logxor st one other (ash one (* 2 (- j i))) (ash other (* 2 (- i j))))))))
     (iter (with len = (length masks))
          (for i from 0 to (- len 2))
          (iter (with mx-ind = i)
                (with mx = 0)
                (for j from i to (- len 1))
                (for curr = (ash (logand (aref masks j) st) (- (* j 2))))
                 (when (> curr mx)
                  (setf mx curr 
                        mx-ind j))
                (finally (swap i mx-ind))))
    st))

(defun enqueue-neighbours (state masks q chip-mask generator-mask seen)
  (let* ((elevator (get-elevator state))
         (flr (get-floor state elevator num-chips))
         (nbr-flr (get-floor state (+ elevator direction) num-chips)))
    ;; (format t "STATE ~16,'0,,16B~%" (logand state (- (ash 1 60) 1)))
    (labels ((next-state (current-elevator next-elevator mask)
               ;; (format t "flr ~4,'0,,4B nbr-flr ~4,'0,,4B mask ~4,'0,,4B~%" flr nbr-flr mask)
               (-<> (set-floor state elevator num-chips (logxor mask flr))
                    (set-floor <> (+ elevator direction) num-chips (logxor mask nbr-flr))
                    (set-elevator current-elevator next-elevator <>))))
      (iter (for bth in both)
            (when (= (logand bth flr) bth)
              (for nxt = (next-state elevator (+ elevator direction) bth))
              (when (not (gethash nxt seen))
                (setf (gethash nxt seen) t)
                (enqueue (list (+ num-moves 1) nxt 'both
                               (set-elevator (get-elevator state) 0 state)
                               bth) q))))
      (iter (for other in others)
            ;; (format t "CANDIDATE ~4,'0,,4B~%" other)
            (when (and (= (logand other flr) other)
                       (safe-floor (logxor flr other) generator-mask chip-mask)
                       (safe-floor (logxor nbr-flr other) generator-mask chip-mask))
              (for nxt = (next-state elevator (+ elevator direction) other))
              (when (not (gethash nxt seen))
                (setf (gethash nxt seen) t)
                (enqueue (list (+ num-moves 1) nxt 'other 
                               (set-elevator (get-elevator state) 0 state)
                               other) q)))))))

(defun explore (start-state num-chips target)
  (let ((seen (make-hash-table))
        (both (choose-both num-chips))
        (others (append (choose-only-one num-chips) (choose-two-different num-chips)))
        (q (make-queue (list 0 start-state 'start 0 0)))
        (chip-mask (make-chip-mask num-chips))
        (generator-mask (make-generator-mask num-chips)))
    (iter (while (non-empty q))
          (for (num-moves state tp prev msk) = (poll q))
          ;; (format t (format 
          ;;            nil "NUM-MOVES ~a elevator ~a STATE ~16,'0,,16B TYPE ~a PREV ~16,'0,,16B MASK ~4,'0,,4B~%" 
          ;;            num-moves
          ;;            (get-elevator state) 
          ;;            (logxor (ash (get-elevator state) 60) state)
          ;;            tp
          ;;            prev
          ;;            msk))
          (when (= state target)
            (leave (cons num-moves state)))
          (when (< (get-elevator state) 3)
            (enqueue-neighbours num-moves state num-chips 1 both others q chip-mask
                                generator-mask seen))
          (when (> (get-elevator state) 0)
            (enqueue-neighbours num-moves state num-chips -1 both others q chip-mask
                                generator-mask seen)))))

(declaim (optimize (speed 3) (debug 0)))

(defun test ()
  (let ((start-state #b000101001010)
        (num-chips 2)
        (target #b0011000000000000000000000000000000000000000000001111000000000000))
    (explore start-state num-chips target)))

(defun part-one ()
  (let ((start-state #b0000000000000000000000000000000000001010101000010101011100000000)
        (num-chips 5)
        (target #b0011000000000000000000001111111111000000000000000000000000000000))
    (explore start-state num-chips target)))
