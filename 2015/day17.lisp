(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :iterate))

(defpackage :day17
  (:use :cl :iterate))

(in-package :day17)

(defun read-containers ()
  (iter (for num in-file "input17")
        (collect num)))

(defun egg-nog (liters containers)
  (cond ((= liters 0) 1)
        ((< liters 0) 0)
        ((null containers) 0)
        ((+ (egg-nog (- liters (car containers)) (cdr containers))
            (egg-nog liters (cdr containers))))))

(defun part-one ()
  (egg-nog 150 (read-containers)))

(defun minimum-containers (liters containers)
  (let ((container-counts (make-hash-table)))
    (labels ((rec (liters containers selected-containers)
               (cond ((= liters 0) 
                      (setf (gethash selected-containers container-counts)
                            (+ 1 (gethash selected-containers container-counts 0))))
                     ((and containers (> liters 0))
                      (rec (- liters (car containers)) (cdr containers) (+ selected-containers 1))
                      (rec liters (cdr containers) selected-containers)))))
      (rec liters containers 0)
      (iter (for (k v) in-hashtable container-counts)
            (for e = (cons k v))
            (reducing e by (lambda (one other) (if (< (car one) (car other)) one other)) 
                      into res)
            (finally (return (cdr res)))))))

(defun part-two ()
  (minimum-containers 150 (read-containers)))
