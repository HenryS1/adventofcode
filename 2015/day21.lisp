(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :iterate)
  (ql:quickload :cl-ppcre)
  (ql:quickload :hylo))

(defpackage :day21
  (:use :cl :iterate :cl-ppcre :monad))

(in-package :day21)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "\\d+" line)))

(defclass player ()
  ((armour :accessor armour :initarg :armour)
   (damage :accessor damage :initarg :damage)
   (hp :accessor hp :initarg :hp)))

(defclass item ()
  ((name :accessor name :initarg :name)
   (cost :accessor cost :initarg :cost)
   (item-damage :accessor item-damage :initarg :item-damage)
   (item-armour :accessor item-armour :initarg :item-armour)))

(defmethod fight ((you player) (boss player))
  (labels ((rec ()
             (let ((you-hp (hp you))
                   (boss-hp (hp boss)))
               (decf (hp boss) (max 1 (- (damage you) (armour boss))))
               (let ((result (if (<= (hp boss) 0)
                                 t
                                 (progn (decf (hp you) (max 1 (- (damage boss) (armour you))))
                                        (if (<= (hp you) 0) nil (rec))))))
                 (setf (hp you) you-hp (hp boss) boss-hp)
                 result))))
    (rec)))

(defmethod set-buffs ((you player) items)
  (iter (for item in items)
        (incf (armour you) (item-armour item))
        (incf (damage you) (item-damage item))))

(defmethod unset-buffs ((you player) items)
  (iter (for item in items)
        (decf (armour you) (item-armour item))
        (decf (damage you) (item-damage item))))

(defun read-item (str)
  (let (*read-eval*)
    (with-input-from-string (s str)
      (destructuring-bind (name cost damage armour)
          (iter (for sym in-stream s)
                (collect sym))
        (make-instance 'item :name name :cost cost :item-damage damage :item-armour armour)))))

(defparameter *weapons*
  (mapcar #'read-item
          (list "Dagger        8     4       0"
                "Shortsword   10     5       0"
                "Warhammer    25     6       0"
                "Longsword    40     7       0"
                "Greataxe     74     8       0")))

(defparameter *armour*
  (mapcar #'read-item
          (list "Leather      13     0       1"
                "Chainmail    31     0       2"
                "Splintmail   53     0       3"
                "Bandedmail   75     0       4"
                "Platemail   102     0       5")))

(defparameter *rings*
  (mapcar #'read-item
          (list "Damage+1    25     1       0"
                "Damage+2    50     2       0"
                "Damage+3   100     3       0"
                "Defense+1   20     0       1"
                "Defense+2   40     0       2"
                "Defense+3   80     0       3")))

(defun find-best-cost (weapons armour rings you boss cmp fight-fun)
  (let (best)
    (labels ((rec (wc ac rc ws as rs cost)
               (cond (wc (when (or (not best)
                                (funcall cmp cost best))
                        (let ((items (append wc ac rc)))
                          (set-buffs you items)
                          (let ((result (funcall fight-fun you boss)))
                            (when result (setf best cost)))
                          (unset-buffs you items))))                     
                     (rs (when (< (length rc) 2)
                           (rec wc ac (cons (car rs) rc) ws as (cdr rs) (+ cost (cost (car rs)))))
                         (rec wc ac rc ws as (cdr rs) cost))
                     (as
                      (when (not ac)
                        (rec wc (cons (car as) ac) rc ws nil rs (+ cost (cost (car as)))))
                      (rec wc ac rc ws (cdr as) rs cost))
                     (ws (rec (list (car ws)) ac rc nil as rs (+ cost (cost (car ws))))
                         (rec wc ac rc (cdr ws) as rs cost)))))
      (rec nil nil nil weapons armour rings 0)
      best)))

(defun read-boss ()
  (destructuring-bind (hp damage armour)
      (iter (for line in-file "input21" using #'read-line)
                  (collect (car (ints line))))
    (make-instance 'player :hp hp :damage damage :armour armour)))

(defun part-one ()
  (let ((you (make-instance 'player :hp 100 :damage 0 :armour 0))
        (boss (read-boss)))
    (find-best-cost *weapons* *armour* *rings* you boss #'< #'fight)))

(defun part-two ()
  (let ((you (make-instance 'player :hp 100 :damage 0 :armour 0))
        (boss (read-boss)))
    (find-best-cost *weapons* *armour* *rings* you boss #'> 
                    (lambda (you boss) (not (fight you boss))))))
