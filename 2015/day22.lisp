(load "../2018/priority-queue.lisp")

(defun me (hit-points mana shield-turns recharge-turns)
  (list hit-points mana shield-turns recharge-turns))

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
  (values (cons (car me) (cons (- (cadr me) 229) (list (caddr me) 5)))
          boss))

(defun make-move (move me boss)
  (case move
    (magic-missile (magic-missile me boss))
    (drain (drain me boss))
    (shield (shield me boss))
    (poison (poison me boss))
    (recharge (recharge me boss))))

(defun next-state (move me boss turns)
  (multiple-value-bind (n-me n-boss) (make-move move me boss)
    (destructuring-bind (me-hp mn sh rech) n-me
      (destructuring-bind (b-hp d p) n-boss
        (when (> p 0) (progn (decf b-hp 3) (decf p)))
        (when (<= b-hp 0) (return-from next-state (list n-me n-boss (+ turns 1))))
        (when (> sh 0) (decf sh))
        (when (> 5 rech 0) (progn (incf mn 101) (decf rech)))
        (when (> sh 0) (decf sh))
        (when (> p 0) (progn (decf b-hp 3) (decf p)))
        (when (<= b-hp 0) (return-from next-state (list n-me n-boss (+ turns 1))))
        (when (> 5 rech 0) (progn (incf mn 101) (decf rech)))
        (if (> sh 0) 
            (progn (decf sh) (decf me-hp (max (- d 7) 1)))
            (decf me-hp d))
        (list (me me-hp mn sh rech)
              (boss b-hp d p)
              (+ turns 1))))))

(defvar *moves* '((recharge . 229) (poison . 173) (shield . 113) (drain . 73) (magic-missile . 53)))

(defun dropwhile (p l) 
  (loop for rest = l then (cdr rest) while (and rest (funcall p (car rest)))
     finally (return rest)))

(defun available-moves (me)
  (let ((mn (cadr me)))
    (mapcar #'car (dropwhile (lambda (x) (< mn (cdr x))) *moves*))))

(defun next-states (boss me turns)
  (let ((available (available-moves me)))
    (mapcar (lambda (move) (next-state move me boss turns)) available)))

(defun explore (q)
  (loop for current = (pop-pq q)
     while current
     do (format t "CURRENT ~a~%" current)
     do (destructuring-bind (me boss turns) current
          (if (<= (car boss) 0)
              (return-from explore turns)
              (when (> (car me) 0)
                (let ((next (next-states boss me turns)))
                  (dolist (n next) (insert-pq n q))))))))

(defparameter *test-state* (list (me 10 250 0 0) (boss 13 8 0) 0))

(defun blah (state move)
  (destructuring-bind (me boss turns) state
    (next-state move me boss turns)))

(defun compare-states (one other)
  (< (caar one) (caar other)))

(defun test ()
  (let ((q (make-pq #'compare-states)))
    (insert-pq *test-state* q)
    (explore q)))
