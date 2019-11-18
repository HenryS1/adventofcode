(defun me (hit-points mana shield-turns recharge-turns)
  (list hit-points mana armour recharge-turns shield-turns))

(defun boss (hit-points damage poison-turns)
  (list hit-points damage poison-turns))

(defun take (n l)
  (if (= n 0) nil (cons (car l) (take (- n 1) (cdr l)))))

(defun magic-missile (me boss)
  (values (cons (car me) (cons (- (cadr me) 53) (cddr me)))
          (cons (- (car boss) 4) (cdr boss))))

(defun drain (me boss)
  (values (cons (+ (car me) 2) (cons (- (cadr me) 73) (cddr me)))
          (cons (- (car boss) 2) (cdr boss))))

(defun shield (me boss)
  (values (cons (car me) (cons (- (cadr me) 113) (cons 6 (cdddr me))))
          boss))

(defun poison (me boss)
  (values (cons (car me) (cons (- (cadr me) 173) (cddr me)))
          (list (car boss) (cadr boss) 6)))

(defun recharge (me boss)
  (values (cons (car me) (cons (- (cadr me) 229)) (cons (caddr me) 5))
          boss))

(defun make-move (move me boss)
  (case move
    'magic-missile (magic-missile me boss)
    'drain (drain me boss)
    'shield (shield me boss)
    'poison (poison me boss)
    'recharge (recharge me boss)))

(defun next-state (move me boss)
  (multiple-value-bind (n-me n-boss) (make-move move me boss)
    (destructuring-bind (me-hp mn sh rech) n-me
      (destructuring-bind (b-hp d p) n-boss
        (when (<= b-hp 0) (return (values n-me n-boss)))
        (decf sh)
        (when (> 5 rech 0) (progn (incf mn 101) (decf rech)))
        (when (> sh 0) (decf sh))
        (when (> p 0) (progn (decf b-hp 3) (decf p)))
        (when (<= b-hp 0) (return (values n-me n-boss)))
        (when (> 5 rech 0) (progn (incf mn 101) (decf rech)))
        (if (> sh 0) 
            (progn (decf sh) (decf m-hp (max (- d 7) 1)))
            (decf m-hp d))))))

(defvar *moves* '((recharge . 229) (poison . 173) (shield . 113) (drain . 73) (magic-missile . 53)))

(defun available-moves (me)
  (let ((mn (cadr me)))
    (mapcar #'car (remove-if-not (lambda (mv) (> mn (cdr mv))) *moves*))))

(defun next-states (boss me)
  (let (())))
