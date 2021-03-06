(load "../2018/queue.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload :metabang-bind)
(ql:quickload :iterate)
(ql:quickload :alexandria)
(ql:quickload :anaphora)

(defpackage :day6
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day6)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun parse-edge (line)
  (read-syms line "\\)"))

(defun make-graph ()
  (iter (with graph = (make-hash-table :test 'equal))
        (for line in-file "input6" using #'read-line)
        (for (one other) = (parse-edge line))
        (setf (gethash other graph) one)
        (finally (return graph))))

(defun count-orbits (graph)
  (let ((cache (make-hash-table :test 'equal)))
    (labels ((rec (one)
               (acond ((equal one 'COM) 0)
                      ((gethash one cache) it)
                      (t (let ((result (+ 1 (rec (gethash one graph)))))
                           (setf (gethash one cache) result)
                           result)))))
      (iter (for (one other) in-hashtable graph)
            (sum (rec one))))))

(defun answer-1 () (count-orbits (make-graph)))

(defun make-bigraph ()
  (iter (with bigraph = (make-hash-table :test 'equal))
        (with santa = nil)
        (with you = nil)
        (for (one other) in-hashtable (make-graph))
        (cond ((equal one 'YOU)
               (setf you other))
              ((equal other 'YOU)
               (setf you one))
              ((equal one 'SAN)
               (setf santa other))
              ((equal other 'SAN)
               (setf santa one))
              (t (push other (gethash one bigraph))
                 (push one (gethash other bigraph))))
        (finally (return (list bigraph you santa)))))

(defun explore (bigraph you santa)
  (let ((seen (make-hash-table :test 'equal))
        (transfers-to (make-hash-table :test 'equal))
        (q (make-queue you)))
    (setf (gethash you transfers-to) 0)
    (setf (gethash you seen) t)
    (iter (while (non-empty q))
          (for current = (poll q))
          (for transfers = (gethash current transfers-to))
          (when (equal current santa)
            (return-from explore (gethash current transfers-to)))
          (iter (for other in (gethash current bigraph))
                (when (not (gethash other seen))
                  (enqueue other q)
                  (setf (gethash other seen) t)
                  (setf (gethash other transfers-to) 
                        (+ transfers 1)))))))

(defun answer-2 ()
  (bind (((bigraph you santa) (make-bigraph)))
    (explore bigraph you santa)))
