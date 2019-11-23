(load "../2018/priority-queue.lisp")

(defun me (hit-points mana shield-turns recharge-turns)
  (list hit-points mana shield-turns recharge-turns))

(defun boss (hit-points damage poison-turns)
  (list hit-points damage poison-turns))

(defun take (n l)
  (if (= n 0) nil (cons (car l) (take (- n 1) (cdr l)))))

(defun magic-missile (me boss mana-used)
  (values (cons (car me) (cons (- (cadr me) 53) (cddr me)))
          (cons (- (car boss) 4) (cdr boss))
          (+ mana-used 53)))

(defun drain (me boss mana-used)
  (values (cons (+ (car me) 2) (cons (- (cadr me) 73) (cddr me)))
          (cons (- (car boss) 2) (cdr boss))
          (+ mana-used 73)))

(defun shield (me boss mana-used)
  (values (cons (car me) (cons (- (cadr me) 113) (cons 6 (cdddr me))))
          boss
          (+ mana-used 113)))

(defun poison (me boss mana-used)
  (values (cons (car me) (cons (- (cadr me) 173) (cddr me)))
          (list (car boss) (cadr boss) 6)
          (+ mana-used 173)))

(defun recharge (me boss mana-used)
  (values (cons (car me) (cons (- (cadr me) 229) (list (caddr me) 5)))
          boss
          (+ mana-used 229)))

(defun make-move (move me boss mana-used)
  (case move
    (magic-missile (magic-missile me boss mana-used))
    (drain (drain me boss mana-used))
    (shield (shield me boss mana-used))
    (poison (poison me boss mana-used))
    (recharge (recharge me boss mana-used))))

(defun next-state (move me boss mana)
  (multiple-value-bind (n-me n-boss mana-used) (make-move move me boss mana)
    (destructuring-bind (me-hp mn sh rech) n-me
      (destructuring-bind (b-hp d p) n-boss
;        (when (equal move 'poison) (format t "P IS ~a~%" p))
        (when (> 6 p 0) (decf b-hp 3))
        (when (> p 0) (decf p))
        (when (<= b-hp 0) (return-from next-state (list (me me-hp mn sh rech)
                                                        (boss b-hp d p)
                                                        mana-used)))
        (when (> sh 0) (decf sh))
        (when (> 5 rech 0) (incf mn 101) (decf rech))
;        (when (equal move 'poison) (format t "P IS NOW ~a~%" p))
        (when (> p 0) (progn (decf b-hp 3) (decf p))
;              (format t "B-HP IS NOW ~a~%" b-hp)
              )
        (when (<= b-hp 0) (return-from next-state (list (me me-hp mn sh rech)
                                                        (boss b-hp d p)
                                                        mana-used)))
        (when (> rech 0) (progn (incf mn 101) (decf rech)))
        (if (> sh 0) 
            (progn (decf sh) (decf me-hp (max (- d 7) 1)))
            (decf me-hp d))
        (list (me me-hp mn sh rech)
              (boss b-hp d p)
              mana-used)))))

(defvar *moves* '((recharge . 229) (poison . 173) (shield . 113) (drain . 73) (magic-missile . 53)))

(defun dropwhile (p l) 
  (loop for rest = l then (cdr rest) while (and rest (funcall p (car rest)))
     finally (return rest)))

(defun not-available (me boss move-mana)
  (destructuring-bind (move . mana) move-mana
    (destructuring-bind (me-hp mn sh rech) me
      (declare (ignore me-hp mn))
      (destructuring-bind (b-hp d p) boss
        (declare (ignore b-hp d))
        (or (< (cadr me) mana)
            (case move 
              (recharge (> rech 0))
              (shield (> sh 0))
              (poison (> p 0))
              (t nil)))))))

(defun available-moves (me boss)
  (mapcar #'car (dropwhile (lambda (x) (not-available me boss x)) *moves*)))

(defun next-states (boss me mana-used)
  (let ((available (available-moves me boss)))
    (mapcar (lambda (move) (next-state move me boss mana-used)) available)))

(defun explore (q)
  (loop for current = (pop-pq q)
     while current
     do (destructuring-bind (me boss mana-used) current
          (if (<= (car boss) 0)
              (return-from explore mana-used)
              (let ((next (next-states boss me mana-used)))
                  (dolist (n next)
                    (destructuring-bind (n-me n-boss n-mn) n
                      (declare (ignore n-boss n-mn))
                      (when (> (car n-me) 0)
                        (insert-pq n q)))))))))

(defparameter *test-state* (list (me 10 250 0 0) (boss 14 8 0) 0))

(defun blah (state move)
  (destructuring-bind (me boss mana-used) state
    (next-state move me boss mana-used)))

(defun compare-states (one other)
  (< (caddr one) (caddr other)))

(defun test ()
  (let ((q (make-pq #'compare-states)))
    (insert-pq *test-state* q)
    (explore q)))

(defun test-pq ()
  (let ((q (make-pq #'<)))
    (loop for i in '(229 173 113 73 53)
       do (insert-pq i q)
         (format t "PQ ~a~%" q))
    (pop-pq q)))

(defun answer-1 ()
  (let ((q (make-pq #'compare-states)))
    (insert-pq (list (me 50 500 0 0) (boss 55 8 0) 0) q)
    (explore q)))
