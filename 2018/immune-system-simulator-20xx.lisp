(ql:quickload :cl-ppcre)
(load "utilities.lisp")

(defun read-army-group (line side)
  (multiple-value-bind (m rs) 
      (cl-ppcre:scan-to-strings
       "(\\d+).*with (\\d+).*\\((.*)\\).*does (\\d+) (\\w+).*initiative (\\d+)" line)
    (if m
        (make-army-group rs side)
        (multiple-value-bind (m rs)
            (cl-ppcre:scan-to-strings
             "(\\d+).*with (\\d+).*does (\\d+) (\\w+).*initiative (\\d+)" line)
          (assert m)
          (make-army-group 
           (vector (aref rs 0) (aref rs 1) "" (aref rs 2) (aref rs 3) (aref rs 4))
           side)))))

(defclass army-group ()
  ((side :accessor side :initarg :side)
   (units :accessor units :initarg :units)
   (health :accessor health :initarg :health)
   (damage :accessor damage :initarg :damage)
   (damage-type :accessor damage-type :initarg :damage-type)
   (initiative :accessor initiative :initarg :initiative)
   (weaknesses :accessor weaknesses :initarg :weaknesses)
   (immunities :accessor immunities :initarg :immunities)))

(defun copy-army (army)
  (make-instance 'army-group :side (side army) :units (units army)
                 :health (health army) :damage (damage army)
                 :damage-type (damage-type army) :initiative (initiative army)
                 :weaknesses (weaknesses army) :immunities (immunities army)))

(defun print-army-group (army-group)
  (loop for sym in (list 'side 'units 'health 'damage 'damage-type 'initiative)
     do (format t "~a ~a~%" sym (funcall sym army-group)))
  (format t "WEAKNESSES:~%")
  (print-hash-table (weaknesses army-group))
  (format t "IMMUNITIES:~%")
  (print-hash-table (immunities army-group)))

(defun parse-weaknesses (str)
  (multiple-value-bind (m rs) 
      (cl-ppcre:scan-to-strings "weak to ((?:\\w+, )*(?:\\w+))" str)
    (if m
        (make-set (mapcar (lambda (tok) (intern (string-upcase tok)))
                      (cl-ppcre:split ",\\s+" (aref rs 0))))
        (make-hash-table))))

(defun parse-immunities (str)
  (multiple-value-bind (m rs) 
      (cl-ppcre:scan-to-strings "immune to ((?:\\w+, )*(?:\\w+))" str)
    (if m
        (make-set (mapcar (lambda (tok) (intern (string-upcase tok)))
                      (cl-ppcre:split ",\\s+" (aref rs 0))))
        (make-hash-table))))

(defun make-army-group (stats side)
  (let ((units (parse-integer (aref stats 0)))
        (health (parse-integer (aref stats 1)))
        (damage (parse-integer (aref stats 3)))
        (damage-type (intern (string-upcase (aref stats 4))))
        (initiative (parse-integer (aref stats 5)))
        (weaknesses (parse-weaknesses (aref stats 2)))
        (immunities (parse-immunities (aref stats 2))))
    (make-instance 'army-group :side side :units units :health health
                     :damage damage :damage-type damage-type
                     :initiative initiative :weaknesses weaknesses
                     :immunities immunities)))

(defun read-armies (filename)
  (with-open-file (f filename)
    (when f
      (loop with side = nil
         with armies = (make-hash-table :test 'equal)
         for line = (read-line f nil nil)
         while line
         when (> (length line) 0)
         do (cond ((search "Immune" line)
                   (setf side 'immune))
                  ((search "Infection" line)
                   (setf side 'infection))
                  (t (push (read-army-group line side) (gethash side armies))))
         finally (return (progn (setf (gethash 'immune armies)
                                      (coerce (gethash 'immune armies) 'vector))
                                (setf (gethash 'infection armies)
                                      (coerce (gethash 'infection armies) 'vector))
                                armies))))))

(defparameter *input-file* "immune-system-simulator-20xx-input.txt")

(defun damage-dealt (attacker defender)
  (cond ((gethash (damage-type attacker) (immunities defender)) 0)
        ((gethash (damage-type attacker) (weaknesses defender)) 
         (* 2 (units attacker) (damage attacker)))
        (t (* (units attacker) (damage attacker)))))

(defun effective-power (army)
  (* (units army) (damage army)))

(defun target-selection-order (attacker)
  (lambda (one other)
    (or (> (damage-dealt attacker one) (damage-dealt attacker other))
        (and 
         (= (damage-dealt attacker one) (damage-dealt attacker other))
         (or (> (effective-power one) (effective-power other))
             (and (= (effective-power one) (effective-power other))
                  (> (initiative one) (initiative other))))))))

(defun army-to-string (army)
  (format nil "~a ~a ~a" (side army) (units army) (effective-power army)))

(defun identify-target (attacker defenders already-taken)
  (loop with ordered = (progn 
                         (sort defenders (target-selection-order attacker))
                         defenders)
     for enemy across ordered
     when (and (> (damage-dealt attacker enemy) 0)
               (not (gethash enemy already-taken)))
     return enemy))

(defun assign-targets (attackers defenders attacking)
  (loop with ordered = (progn 
                         (sort attackers
                               (lambda (one other) 
                                 (or (> (effective-power one) 
                                        (effective-power other))
                                     (and 
                                      (= (effective-power one)
                                         (effective-power other))
                                      (> (initiative one)
                                         (initiative other))))))
                         attackers)
     with already-taken = (make-hash-table)
     for attacker across ordered 
     for defender = (identify-target attacker defenders already-taken)
     when defender
     do (setf (gethash attacker attacking) defender)
       (setf (gethash defender already-taken) t)))

(defun print-attacking (attacking)
  (loop for attacker being the hash-keys of attacking using (hash-value defender)
     do (format t "~a ~a is attacking ~a ~a~%" 
                (side attacker) (units attacker)
                (side defender) (units defender))))

(defun assign-all-targets (one-side other-side)
  (let ((attacking (make-hash-table)))
    (assign-targets one-side other-side attacking)
    (assign-targets other-side one-side attacking)
    attacking))

(defun damage-enemy (attacker defender)
  (let* ((attack-damage (damage-dealt attacker defender))
         (units-killed (min (units defender)
                            (floor attack-damage (health defender)))))
    (decf (units defender) units-killed)))

(defun dead (army)
  (= (units army) 0))

(defun print-armies (armies)
  (format t "IMMUNE SYSTEM:~%")
  (loop for army across (gethash 'immune armies)
     do (print-army-group army))
  (format t "INFECTION:~%")
  (loop for army across (gethash 'infection armies)
     do (print-army-group army)))

(defun remove-dead-units (armies)
  (setf (gethash 'immune armies) (remove-if #'dead (gethash 'immune armies)))
  (setf (gethash 'infection armies) (remove-if #'dead (gethash 'infection armies))))

(defun fight-round (armies)
  (loop with attacking = (assign-all-targets (gethash 'immune armies) 
                                             (gethash 'infection armies))
     with attack-order = (sort (concatenate 'vector (gethash 'immune armies)
                                            (gethash 'infection armies))
                               (lambda (one other) (> (initiative one) (initiative other))))
     for attacker across attack-order 
     when (and (> (units attacker) 0)
               (gethash attacker attacking))
     do (damage-enemy attacker (gethash attacker attacking))
     finally (remove-dead-units armies)))

(defun fight-to-end (armies)
  (loop for round = 1 then (1+ round)
     while (and (> (length (gethash 'immune armies)) 0)
                   (> (length (gethash 'infection armies)) 0))
     do (fight-round armies)))

(defun immune-system-wins (armies)
  (and (> (count-units (gethash 'immune armies)) 0)
           (= (count-units (gethash 'infection armies)) 0)))

(defun count-units (side)
  (loop for army across side 
     sum (units army)))

(defun total-units (armies)
  (+ (count-units (gethash 'immune armies))
     (count-units (gethash 'infection armies))))

(defun copy-armies (armies)
  (let ((new-armies (make-hash-table :test 'equal)))
    (setf (gethash 'immune new-armies) (map 'vector #'copy-army (gethash 'immune armies)))
    (setf (gethash 'infection new-armies) (map 'vector #'copy-army (gethash 'infection armies)))
    new-armies))

(defun add-boost (immune-system-armies boost)
  (loop for army across immune-system-armies 
     do (incf (damage army) boost)))

(defun find-min-boost (armies)
  (loop for boost = 1 then (1+ boost)
     for armies-copy = (copy-armies armies)
     do (add-boost (gethash 'immune armies) boost)
     do (fight-to-end armies-copy)
     until (immune-system-wins armies-copy)
     finally (return (count-units (gethash 'immune armies-copy)))))

(defparameter *test-input-file* "immune-system-simulator-20xx-test-input.txt")

(defun test-1 ()
  (let ((armies (read-armies *test-input-file*)))
    (fight-to-end armies)
    (total-units armies)))

(defun solution-part-1 ()
  (let ((armies (read-armies *input-file*)))
    (fight-to-end armies)
    (total-units armies)))

(defun solution-part-2 ()
  (let ((armies (read-armies *input-file*)))
    (find-min-boost armies)))
