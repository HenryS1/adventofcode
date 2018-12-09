(defun read-instructions ()
  (with-open-file (f "bathroom-instructions-input.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line
         collect line))))

(defun move (instruction position)
  (cond ((char= instruction #\L)
         (cons (car position) (max 0 (1- (cdr position)))))
        ((char= instruction #\R)
         (cons (car position) (min 2 (1+ (cdr position)))))
        ((char= instruction #\U)
         (cons (max 0 (1- (car position))) (cdr position)))
        ((char= instruction #\D)
         (cons (min 2 (1+ (car position))) (cdr position)))
        (t (error (format nil "unknown instruction ~a" instruction)))))

;;      1
;;    2 3 4
;;  5 6 7 8 9  
;;    A B C 
;;      D

(defun neighbours (keypad-entry)
  (case keypad-entry
    (#\1 (list nil nil nil #\3))
    (#\2 (list nil nil #\3 #\6))
    (#\3 (list #\2 #\1 #\4 #\7))
    (#\4 (list #\3 nil nil #\8))
    (#\5 (list nil nil #\6 nil))
    (#\6 (list #\5 #\2 #\7 #\A))
    (#\7 (list #\6 #\3 #\8 #\B))
    (#\8 (list #\7 #\4 #\9 #\C))
    (#\9 (list #\8 nil nil nil))
    (#\A (list nil #\6 #\B nil))
    (#\B (list #\A #\7 #\C #\D))
    (#\C (list #\B #\8 nil nil))
    (#\D (list nil #\B nil nil))))

(defun move-on-diamond-keypad (instruction current-entry)
  (case instruction
    (#\L (car (neighbours current-entry)))
    (#\U (cadr (neighbours current-entry)))
    (#\R (caddr (neighbours current-entry)))
    (#\D (cadddr (neighbours current-entry)))))

(defun find-next-diamond-entry (instructions entry)
  (loop for instruction across instructions
     do (let ((next (move-on-diamond-keypad instruction entry)))
          (when next
            (setf entry next))))
  entry)

(defun find-diamond-keypad-passcode (all-instructions)
  (let ((entry #\5))
    (map 'string #'identity (loop for instructions in all-instructions 
                               do (let ((next-entry (find-next-diamond-entry instructions entry)))
                                    (when next-entry
                                      (setf entry next-entry)))
                               collect entry))))

(defun find-next-position (instructions position)
  (loop for instruction across instructions
     do (setf position (move instruction position)))
  position)

(defun find-keypad-positions (all-instructions)
  (let ((position (cons 1 1)))
    (loop for instructions in all-instructions
       do (setf position (find-next-position instructions position))
       collect position)))

(defparameter *keypad* (make-array '(3 3) :initial-contents 
                                   '((1 2 3)
                                     (4 5 6)
                                     (7 8 9))))

(defun keypad-value (position)
  (aref *keypad* (car position) (cdr position)))

(defun find-passcode (all-instructions)
  (mapcar #'keypad-value (find-keypad-positions all-instructions)))

(defun solution-part-1 ()
  (find-passcode (read-instructions)))

(defun solution-part-2 ()
  (find-diamond-keypad-passcode (read-instructions)))
