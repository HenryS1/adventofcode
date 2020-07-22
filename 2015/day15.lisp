(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-ppcre)
  (ql:quickload :metabang-bind)) 

(defpackage :day15
  (:use :cl :cl-ppcre :metabang-bind))

(in-package :day15)

(defclass ingredient ()
  ((name :accessor name :initarg :name)
   (capacity :accessor capacity :initarg :capacity)
   (durability :accessor durability :initarg :durability)
   (flavor :accessor flavor :initarg :flavor)
   (texture :accessor texture :initarg :texture)
   (calories :accessor calories :initarg :calories)))

(defun parse-ingredient (line)
  (bind (((:values _ v) 
          (scan-to-strings
           "(\\w+):[^\\d-]+(-?\\d+)[^\\d-]+(-?\\d+)[^\\d-]+(-?\\d+)[^\\d-]+(-?\\d+)[^\\d-]+(-?\\d+).*" line))
         (#(name capacity durability flavor texture calories) v))
    (make-instance 'ingredient :name (intern (string-upcase name)) 
                   :capacity (parse-integer capacity)
                   :durability (parse-integer durability)
                   :flavor (parse-integer flavor)
                   :texture (parse-integer texture) 
                   :calories (parse-integer calories))))

(defun score-cookie (allocations ingredients)
  (loop for alloc in allocations 
     for ingredient in ingredients
     sum (* alloc (capacity ingredient)) into cap
     sum (* alloc (durability ingredient)) into dura
     sum (* alloc (flavor ingredient)) into flav
     sum (* alloc (texture ingredient)) into tex
     finally (return (* (max cap 0) (max dura 0) (max flav 0) (max tex 0)))))

(defun total-calories (allocations ingredients)
  (reduce #'+ (mapcar (lambda (alloc i) (* alloc (calories i))) allocations ingredients)))

(defun find-optimal-cookie (initial-ingredients initial-tablespoons 
                            &optional (calories-target nil))
  (let ((best 0))
    (labels ((rec (ingredients tablespoons current-count acc)
               (cond ((null (cdr ingredients)) 
                      (let* ((allocations (reverse (push tablespoons acc)))
                             (score (score-cookie allocations initial-ingredients)))
                        (when (or (not calories-target) 
                                  (= (total-calories allocations initial-ingredients) 
                                     calories-target)) 
                          (setf best (max score best)))))
                     ((= tablespoons 0) 
                      (let* ((allocations
                              (append (reverse acc)
                                      (mapcar (lambda (i) (declare (ignore i)) 0) ingredients)))
                             (score (score-cookie allocations initial-ingredients)))
                        (when (or (not calories-target) 
                                  (= (total-calories allocations initial-ingredients) 
                                     calories-target)) 
                          (setf best (max score best)))))
                     (t (rec ingredients (- tablespoons 1) (+ current-count 1) acc)
                        (rec (cdr ingredients) tablespoons 0 (cons current-count acc))))))
      (rec initial-ingredients initial-tablespoons 0 nil))
    best))

(defun read-lines ()
  (with-open-file (f "input15") (loop for line = (read-line f nil nil) while line collect line)))

(defun part-one ()
  (let* ((ingredients (mapcar #'parse-ingredient (read-lines))))
    (find-optimal-cookie ingredients 100)))

(defun part-two ()
  (let* ((ingredients (mapcar #'parse-ingredient (read-lines))))
    (find-optimal-cookie ingredients 100 500)))
