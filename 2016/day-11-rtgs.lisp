(load "../2018/priority-queue.lisp")

(ql:quickload :cl-ppcre)

(defun collect-equipment (syms)
  (loop for ss = syms then (cdr ss)
     while (cdr ss)
     when (eq (cadr ss) 'compatible)
     collect (list 'chip (car ss))
     when (eq (cadr ss) 'generator)
     collect (list 'generator (car ss))))

(defun parse-equipment (line)
  (let ((syms (mapcar (lambda (w) (intern (string-upcase w)))
                      (cl-ppcre:split "[\\s-,.]+" line))))
    (collect-equipment syms)))

(defun read-input ()
  (with-open-file (f "day11.txt")
    (when f
      (loop with floors = (make-hash-table :test 'equal)
         for line = (read-line f nil nil)
         while line 
         for floor-number = 0 then (+ floor-number 1)
         for new-equipment = (parse-equipment line)
         do (setf (gethash floor-number floors) 
                  (append (gethash floor-number floors) new-equipment))
         finally (return (make-instance 'containment-area 
                                        :floors floors
                                        :elevator 0))))))

(defun copy-hash-table (table)
  (let ((new (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k new) (copy-list v))) table)
    new))

(defun hash-from-alist (l)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (k . v) in l do (setf (gethash k table) v))
    table))

(defun print-hash-table (table)
  (maphash (lambda (k v) (format t "~a -> ~a~%" k v)) table))

(defclass containment-area ()
  ((floors :accessor floors :initarg :floors)
   (elevator :accessor elevator :initarg :elevator)))

(defun move-equipment (equipment from to containment-area)
  (push equipment (gethash to (floors containment-area)))
  (setf (gethash from (floors containment-area))
        (remove equipment (gethash from (floors containment-area)))))

(defun copy-containment-area (containment-area)
  (make-instance 'containment-area
                 :floors (copy-hash-table (floors containment-area))
                 :elevator (elevator containment-area)))

(defun chip-has-companion (chip equipment)
  (find-if (lambda (e) (and (eq (car e) 'generator)
                            (eq (cadr e) (cadr chip))))
           equipment))

(defun equipment-safe (equipment)
  (or (not (some (lambda (e) (eq (car e) 'generator)) equipment))
      (every (lambda (e) (or (not (eq (car e) 'chip)) 
                             (chip-has-companion e equipment)))
             equipment)))

(defun state-is-valid (containment-area)
  (every (lambda (floor-number) 
         (equipment-safe (gethash floor-number (floors containment-area))))
       '(0 1 2 3)))

(defun finished-p (containment-area)
  (every (lambda (floor) (null (gethash floor (floors containment-area))))
         '(0 1 2)))

(defun move (move containment-area)
  (let ((current-floor (elevator containment-area)))
    (destructuring-bind (floor-number . equipment) move
      (loop for e in equipment
         do (push e (gethash floor-number (floors containment-area)))
           (setf (gethash current-floor (floors containment-area))
                 (remove e (gethash current-floor (floors containment-area))))
           (setf (elevator containment-area) floor-number)))))

(defun choose-equipment-to-move (equipment)
  (loop with choices = (list)
     for rest = (cons nil equipment) then (cdr rest)
     while (cdr rest)
     do (loop for other = (cdr rest) then (cdr other)
           while other
           when (null (car rest))
           do (push (list (car other)) choices)
           when (and (not (eq (caar rest) (caar other))))
           do (when (eq (cadar rest) (cadar other))
                (push (list (car rest) (car other)) choices))
           when (eq (caar rest) (caar other))
           do (push (list (car rest) (car other)) choices))
     finally (return choices)))

(defun generate-moves (containment-area)
  (let* ((equipment (gethash (elevator containment-area)
                             (floors containment-area)))
         (equipment-choices (choose-equipment-to-move equipment))
         (available-floors (remove (elevator containment-area) '(0 1 2 3))))
    (loop with moves = (list)
       for floor-number in available-floors
       do (loop for choice in equipment-choices
             do (push (cons floor-number choice) moves))
       finally (return moves))))

(defun generate-next-states (distance containment-area)
  (loop for m in (generate-moves containment-area)
     for cpy = (copy-containment-area containment-area)
     do (move m cpy)
     when (state-is-valid cpy)
     collect (cons distance cpy)))

(defun compare-number-of-moves (one-state other-state)
  (< (car one-state) (car other-state)))

;; (defun find-moves (containment-area)
;;   (let ((pq (make-pq #'compare-number-of-moves)))
;;     (insert-pq (cons 0 containment-area) pq)
;;     (loop while (not (empty pq))
;;        for (moves-taken ca) = (pop-pq pq)
;;        until (finished-p ca)
;;        do ())))
