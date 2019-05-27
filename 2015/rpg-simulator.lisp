(defun make-weapon (cost damage)
  (list cost damage 0))

(defun will-win (me boss)
  (destructuring-bind (hp damage armour) me
    (destructuring-bind (boss-hp boss-damage boss-armour) boss
      (>= (/ hp (max (- boss-damage armour) 1)) 
          (/ boss-hp (max (- damage boss-armour) 1))))))

(defun make-player (equipment)
  (destructuring-bind (weapon a left-ring right-ring) equipment
    (let ((damage (+ (damage weapon) (damage left-ring) (damage right-ring)))
          (armour (+ (armour a) (armour left-ring) (armour right-ring))))
      (list 100 damage armour))))

(defun make-player-from-stats (hp damage armour)
  (list hp damage armour))

(defparameter *weapons*
  (list (make-weapon 8 4)
        (make-weapon 10 5)
        (make-weapon 25 6)
        (make-weapon 40 7)
        (make-weapon 74 8)))

(defun make-armour (cost armour)
  (list cost 0 armour))

(defparameter *armour*
  (list (make-armour 13 1)
        (make-armour 31 2)
        (make-armour 53 3)
        (make-armour 75 4)
        (make-armour 102 5)
        (make-armour 0 0)))

(defun make-ring (cost damage armour)
  (list cost damage armour))

(defun cost (equipment)
  (car equipment))

(defun damage (equipment)
  (cadr equipment))

(defun armour (equipment)
  (caddr equipment))

(defparameter *rings*
  (list (make-ring 25 1 0)
        (make-ring 50 2 0)
        (make-ring 100 3 0)
        (make-ring 20 0 1)
        (make-ring 40 0 2)
        (make-ring 80 0 3)
        (make-ring 0 0 0)))

(defun total-cost (equipment)
  (reduce #'+ (mapcar #'cost equipment)))

(defun select-cheapest-equipment (boss)
  (loop for weapon in *weapons*
     with best-cost = nil
     with equipment = nil
     do (loop for armour in *armour*
           do (loop for left-ring in *rings*
                 do (loop for right-ring in *rings*
                       for current-equipment = (list weapon
                                                      armour
                                                      left-ring
                                                      right-ring)
                       when (and (or (= (cost left-ring) 0)
                                     (not (equal left-ring right-ring)))
                                 (will-win (make-player current-equipment) boss)
                                 (or (null best-cost)
                                     (< (total-cost current-equipment) best-cost)))
                       do (setf equipment current-equipment)
                         (setf best-cost (total-cost current-equipment)))))
     finally (return (cons best-cost equipment))))

