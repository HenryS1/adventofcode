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
         finally (return floors)))))

(defun copy-hash-table (table)
  (let ((new (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k new) (copy-list v))) table)))

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
                 :equipment (copy-hash-table (equipment containment-area))
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

(defun finished-p (containment-area)
  (every (lambda (floor) (null (gethash floor (floors containment-area))))
         '(0 1 2)))

(defun move-elevator (floor move containment-area)
  (let ((current-floor (elevator containment-area)))
    (case (car move)
      (both (let ((chip (cadr move))
                  (generator (caddr move)))
              (move-chip chip current-floor floor containment-area)
              (move-generator generator current-floor floor containment-area)))
      (chips (dolist (chip (cdr move)) 
               (move-chip chip current-floor floor containment-area)))
      (generators (dolist (generator (cdr move))
                    (move-generator generator current-floor floor containment-area))))
    containment-area))

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
                (push (cons (car rest) (car other)) choices))
           when (eq (caar rest) (caar other))
           do (push (cons (car rest) (car other)) choices))
     finally (return choices)))

(defun generate-moves (containment-area)
  (let* ((equipment (gethash (elevator containment-area)
                             (floors containment-area)))
         (equipment-choices (choose-equipment-to-move equipment))
         (available-floors (remove (elevator containment-area) '(0 1 2 3))))
    (loop with moves = (list)
       for floor-number in available-floors
       do (loop for choice in equipment-choices
             do ()))))
