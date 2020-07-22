(ql:quickload :cl-ppcre)
(ql:quickload :metabang-bind)

(defpackage :day14
  (:use :cl :bind :cl-ppcre))

(in-package :day14)

(defclass reindeer ()
  ((name :accessor name :initarg :name)
   (spd :accessor spd :initarg :spd)
   (duration :accessor duration :initarg :duration)
   (wait :accessor wait :initarg :wait)
   (waiting :accessor waiting :initform 0)
   (flying :accessor flying :initarg :flying)
   (location :accessor location :initform 0)
   (points :accessor points :initform 0)))

(defun parse-reindeer (line)
  (bind (((:values _ v) 
          (scan-to-strings "([^\\s]+)[^\\d]+(\\d+)[^\\d]+(\\d+)[^\\d]+(\\d+)[^\\d]+" line))
         (#(name speed duration wait) v))
    (make-instance 'reindeer
                   :name (intern (string-upcase name))
                   :spd (parse-integer speed)
                   :duration (parse-integer duration)
                   :wait (parse-integer wait)
                   :flying (parse-integer duration))))

(defmethod tick ((reindeer reindeer))
  (cond ((> (flying reindeer) 0)
         (incf (location reindeer) (spd reindeer))
         (decf (flying reindeer))
         (when (= (flying reindeer) 0)
           (setf (waiting reindeer) (wait reindeer))))
        (t (decf (waiting reindeer))
           (when (= (waiting reindeer) 0)
             (setf (flying reindeer) (duration reindeer))))))

(defun update-points (reindeer)
  (let ((furthest (loop for r in reindeer maximizing (location r))))
    (loop for r in reindeer when (= (location r) furthest) do (incf (points r)))))

(defun read-lines ()
  (with-open-file (f "input14")
    (when f (loop for line = (read-line f nil nil) while line collect line))))

(defun part-one ()
  (let ((reindeer (mapcar #'parse-reindeer (read-lines))))
    (loop for i from 1 to 2503 do (mapc #'tick reindeer))
    (location (car (sort reindeer (lambda (one other) (> (location one) (location other))))))))

(defun part-two ()
  (let ((reindeer (mapcar #'parse-reindeer (read-lines))))
    (loop for i from 1 to 2503 do (mapc #'tick reindeer) (update-points reindeer))
    (points (car (sort reindeer (lambda (one other) (> (points one) (points other))))))))
