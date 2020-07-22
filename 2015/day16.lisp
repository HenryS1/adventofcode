(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-ppcre)
  (ql:quickload :metabang-bind)
  (ql:quickload :iterate)
  (ql:quickload :alexandria))

(defpackage :day16
  (:use :cl :cl-ppcre :bind :iterate :alexandria))

(in-package :day16)

(defun parse-sue (line)
  (let (*read-eval*)
    (with-input-from-string (s (string-upcase (regex-replace-all "[,:]" line "")))
      (iter (with sue = (make-hash-table))
            (for key = (read s nil nil))
            (for value = (read s nil nil))
            (while key)
            (setf (gethash key sue) value)
            (finally (return sue))))))

(defparameter *clue* (parse-sue 
                      "children: 3
                       cats: 7
                       samoyeds: 2
                       pomeranians: 3
                       akitas: 0
                       vizslas: 0
                       goldfish: 5
                       trees: 3
                       cars: 2
                       perfumes: 1"))

(defun match-sue (clue sue)
  (iter (for (k v) in-hashtable clue)
        (when (and (gethash k sue) (not (equal (gethash k sue) v)))
          (leave nil))
        (finally (return t))))

(defun find-sue (clue sues matcher)
  (find-if (lambda (sue) (funcall matcher clue sue)) sues))

(defun read-sues ()
  (iter (for line in-file "input16" using #'read-line)
        (collect (parse-sue line))))

(defun part-one ()
  (find-sue *clue* (read-sues) #'match-sue))

(defun match-sue-two (clue sue)
  (iter (for (k v) in-hashtable clue)
        (when (gethash k sue)
          (cond ((find k '(cats trees))
                 (and (<= (gethash k sue) v) (leave nil)))
                ((find k '(pomeranians goldfish))
                 (and (>= (gethash k sue) v) (leave nil)))
                ((not (= (gethash k sue) v)) 
                 (leave nil))))
        (finally (return t))))

(defun part-two ()
  (gethash 'sue (find-sue *clue* (read-sues) #'match-sue-two)))
