(load "../2018/queue.lisp")
(ql:quickload :cl-ppcre)

(defun collect-equipment (syms)
  (loop for ss = syms then (cdr ss)
     while (cdr ss)
     when (eq (cadr ss) 'compatible)
     collect (list 'm (car ss))
     when (eq (cadr ss) 'generator)
     collect (list 'g (car ss))))

(defun parse-equipment (line)
  (let ((syms (mapcar (lambda (w) (intern (subseq (string-upcase w) 0)))
                      (cl-ppcre:split "[\\s-,.]+" line))))
    (collect-equipment syms)))

(defun read-input ()
  (with-open-file (f "day11.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line 
         collect (parse-equipment line)))))

(defun other-floors (elevator) 
  (loop for i from 0 to 3 when (/= i elevator) collect i))

(defun same-equipment (one other)
  (equal (car one) (car other)))

(defun same-isotope (one other)
  (equal (cadr one) (cadr other)))

(defun safe-to-move (move)
  (destructuring-bind (one other) move
      (or (null one)
          (same-equipment one other)
          (same-isotope one other))))

(defun safe-equipment (equipment)
  (not (find-if
        (lambda (e) 
          (and (equal (car e) 'm)
               (not (find-if (lambda (other)
                               (and (equal (car other) 'g)
                                    (equal (cadr other) (cadr e))))
                             equipment))
               (find-if (lambda (other)
                          (and (equal (car other) 'g)
                               (not (equal (cadr other) (cadr e)))))
                        equipment)))
        equipment)))

(defun add-equipment (move equipment)
  (append move equipment))

(defun set-nth (n e l)
  (if (= n 0)
      (cons e (cdr l))
      (cons (car l) (set-nth (- n 1) e (cdr l)))))

(defun compare-equipment (one other)
  (or (string< (symbol-name (car one))
               (symbol-name (car other)))
      (and 
       (string= (symbol-name (car one))
                (symbol-name (car other)))
       (string< (symbol-name (cadr one))
                (symbol-name (cadr other))))))

(defun sort-equipment (floors)
  (mapcar (lambda (equipment) (sort (copy-seq equipment) #'compare-equipment)) floors))

(defun next-states (moves-to floors elevator)
  (loop with equipment = (nth elevator floors)
     with available = (cons nil equipment)
     with next = (list)
     for ones = available then (cdr ones)
     while (cdr ones)
     do (loop for others = (cdr ones) then (cdr others)
           while others
           for move = (list (car ones) (car others))
           for remaining = (set-difference equipment move)
;           do (format t "ONES ~a OTHERS ~a~%" ones others)
           when (and (safe-to-move move)
                     (safe-equipment remaining))
           do; (format t "MOVE ~a~%" move)
             ;(format t "REMAINING ~a~%" remaining)
             (loop for floor in (other-floors elevator)
                for added = (add-equipment (remove-if #'null move) (nth floor floors))
              ;  do (format t "ADDED ~a~%" added)
                when (safe-equipment added)
                do (push (list (+ moves-to 1) 
                               (sort-equipment
                                (set-nth floor added (set-nth elevator remaining floors)))
                               floor)
                         next)))
     finally (return next)))

(defun finished (floors)
  (destructuring-bind (first second third fourth) floors
    (declare (ignore fourth))
    (and (null first) (null second) (null third))))

(defparameter *test-floors* 
  '(((m h) (m l))
    ((g h))
    ((g l))
    nil))

(defun find-path-to (path-lookup end-state)
  (loop for current = end-state then (gethash current path-lookup)
     while current 
     collect current))

(defun find-min-steps (floors)
;  (format t "FLOORS INIT ~a~%" floors)
  (let ((q (make-queue (list 0 floors 0)))
        (seen (make-hash-table :test 'equal))
        (path-to (make-hash-table :test 'equal))
        (steps 0))
    (setf (gethash (list floors 0) seen) t)
    (loop for (moves-to floors elevator) = (poll q)
       do (incf steps)
 ;        (format t "FLOORS ~a~%" floors)
       until (finished floors)
       do (loop for state in (next-states moves-to floors elevator)
             when (not (gethash (cdr state) seen))
             do (enqueue state q)
;               (format t "CURRENT ~a~% NEXT ~a~%" floors (cdr state))
               (setf (gethash (cdr state) seen) t)
               (setf (gethash (cdr state) path-to) (list floors elevator)))
       finally (return 
                 (progn 
                         (format t "FLOORS ~a~%" floors)
                         (find-path-to path-to (list floors elevator)))
                       ))))
