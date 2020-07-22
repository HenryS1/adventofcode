(eval-when (:compile-toplevel)
  (ql:quickload :metabang-bind)
  (ql:quickload :iterate)
  (ql:quickload :cl-ppcre))

(defpackage :day22
  (:use :cl :bind :iterate :cl-ppcre))

(in-package :day22)

(defclass effect ()
  ((name :accessor name :initarg :name)
   (timer :accessor timer :initarg :timer)
   (forward :accessor forward :initarg :forward)
   (backward :accessor backward :initarg :backward)))

(defclass player ()
  ((hp :accessor hp :initarg :hp)
   (armour :accessor armour :initarg :armour)
   (damage :accessor damage :initarg :damage)
   (mana :accessor mana :initarg :mana)
   (effects :accessor effects :initform nil)))

(defmethod apply-effect ((effect effect) (player player))
  (assert (> (timer effect) 0))
  (decf (timer effect))
  (funcall (forward effect) player))

(defmethod unapply-effect ((effect effect) (player player))
  (incf (timer effect))
  (funcall (backward effect) player))

(defclass spell ()
  ((name :accessor name :initarg :name)
   (cost :accessor cost :initarg :cost)
   (damage :accessor damage :initarg :damage :initform nil)
   (healing :accessor healing :initarg :healing :initform nil)
   (you-effect :accessor you-effect :initarg :you-effect :initform nil)
   (boss-effect :accessor boss-effect :initarg :boss-effect :initform nil)))

(defmethod apply-spell ((spell spell) (you player) (boss player))
  (when (damage spell) (decf (hp boss) (damage spell)))
  (when (healing spell) (incf (hp you) (healing spell)))
  (when (boss-effect spell) (push (funcall (boss-effect spell)) (effects boss)))
  (when (you-effect spell) (push (funcall (you-effect spell)) (effects you))))

(defmethod unapply-spell ((spell spell) (you player) (boss player))
  (when (damage spell) (incf (hp boss) (damage spell)))
  (when (healing spell) (decf (hp you) (healing spell)))
  (when (boss-effect spell) (pop (effects boss)))
  (when (you-effect spell) (pop (effects you))))

(defparameter *spells* 
  (list (make-instance 'spell :name 'magic-missile :cost 53 :damage 4)
        (make-instance 'spell :name 'drain :cost 73 :damage 2 :healing 2)
        (make-instance 'spell :name 'shield :cost 113 
                       :you-effect (lambda ()
                                     (make-instance 
                                      'effect :name 'shield
                                      :timer 6
                                      :forward (lambda (you) (incf (armour you) 7))
                                      :backward (lambda (you) (decf (armour you) 7)))))
        (make-instance 'spell :name 'poison :cost 173
                       :boss-effect (lambda ()
                                      (make-instance
                                       'effect :name 'poison
                                       :timer 6
                                       :forward (lambda (boss) (decf (hp boss) 3))
                                       :backward (lambda (boss) (incf (hp boss) 3)))))
        (make-instance 'spell :name 'recharge :cost 229
                       :you-effect (lambda ()
                                     (make-instance 
                                      'effect :name 'recharge
                                      :timer 5
                                      :forward (lambda (you) (incf (mana you) 101))
                                      :backward (lambda (you) (decf (mana you) 101)))))))

(defun min-cost-win (you boss &optional (hard nil))
  (let (best)
    (labels ((tick (cost your-turn)
               (when (and hard your-turn) (decf (hp you)))
               (when (and (> (hp you) 0) (or (not best) (< cost best)))
                 (let ((you-effects (effects you))
                       (boss-effects (effects boss))
                       (armour-before (armour you)))
                   (setf (armour you) 0)
                   (setf (effects you)
                         (iter (for eff in (effects you))
                               (apply-effect eff you)
                               (when (> (timer eff) 0)
                                 (collect eff))))
                   (setf (effects boss)
                         (iter (for eff in (effects boss))
                               (apply-effect eff boss)
                               (when (> (timer eff) 0)
                                 (collect eff))))
                   (if (<= (hp boss) 0)
                       (setf best cost)
                       (if your-turn 
                           (iter (for spell in *spells*)
                                 (for hp-before = (hp you))
                                 (when (and (< (cost spell) (mana you))
                                            (not (find-if (lambda (eff) 
                                                            (eq (name eff) (name spell)))
                                                          (effects you)))
                                            (not (find-if (lambda (eff) 
                                                            (eq (name eff) (name spell)))
                                                          (effects boss))))
                                   (decf (mana you) (cost spell))
                                   (apply-spell spell you boss)
                                   (tick (+ cost (cost spell)) (not your-turn))
                                   (unapply-spell spell you boss)
                                   (incf (mana you) (cost spell))))
                           (progn
                             (decf (hp you) (max 1 (- (damage boss) (armour you))))
                             (when (> (hp you) 0) 
                               (tick cost (not your-turn)))
                             (incf (hp you) (max 1 (- (damage boss) (armour you)))))))
                   (iter (for eff in you-effects)
                         (unapply-effect eff you))
                   (iter (for eff in boss-effects)
                         (unapply-effect eff boss))
                   (setf (effects you) you-effects)
                   (setf (effects boss) boss-effects)
                   (setf (armour you) armour-before)))
               (when (and hard your-turn) (incf (hp you)))))
      (tick 0 t))
    best))

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "\\d+" line)))

(defun read-boss ()
  (destructuring-bind (hp damage)
      (iter (for line in-file "input22" using #'read-line)
                  (collect (car (ints line))))
    (make-instance 'player :hp hp :damage damage :armour 0)))

(defun part-one ()
  (let ((you (make-instance 'player :hp 50 :mana 500 :armour 0))
        (boss (read-boss)))
    (min-cost-win you boss)))

(defun part-two ()
  (let ((you (make-instance 'player :hp 50 :mana 500 :armour 0))
        (boss (read-boss)))
    (min-cost-win you boss t)))
