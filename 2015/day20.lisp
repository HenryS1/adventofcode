(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind))

(defpackage :day20
  (:use :cl :iterate :anaphora :bind))

(in-package :day20)

(defun memoize (fun)
  (let ((old-fun (symbol-function fun))
        (table (make-hash-table :test 'equal)))
    (setf (symbol-function fun) 
          (lambda (&rest args)
            (multiple-value-bind (v found) (gethash args table)
              (if found
                  v
                  (let ((res (apply old-fun args)))
                    (setf (gethash args table) res)
                    res)))))))

(defun factor-sum (n)
  (if (= n 1)
      1
      (bind ((smallest-prime-factor (iter (for i from 2)
                                          (while (<= (* i i) n))
                                          (when (= (mod n i) 0) (return i))
                                          (finally (return n))))
             ((:values divided-out prime-power)
              (iter (for d first n then (floor d smallest-prime-factor))
                    (for pow first 1 then (* smallest-prime-factor pow))
                    (while (= (mod d smallest-prime-factor) 0))
                    (finally (return (values d pow))))))
        (+ (factor-sum (floor n smallest-prime-factor)) 
           (* prime-power (factor-sum divided-out))))))

(memoize 'factor-sum)

(defun part-one ()
  (iter (for i from 1) 
        (for presents = (* 10 (factor-sum i)))
        (while (< presents 29000000))
        (finally (return i))))

(defun less-than-fifty (n)
  (iter (for i from 1)
        (while (and (<= i 50)))
        (when (= (mod n i) 0)
          (sum  (floor n i)))))

(defun part-two ()
  (iter (for i from 1)
        (for presents = (* 11 (less-than-fifty i)))
        (while (< presents 29000000))
        (finally (return i))))
