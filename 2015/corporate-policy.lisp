(defun increment-char (c)
  (let ((code (+ (char-code c) 1)))
    (if (= code 123)
        (setf code 97))
    (code-char code)))

(defun increment-password (password)
  (labels ((recur (password index)
             (let ((incremented (increment-char (aref password index)))) 
               (setf (aref password index) incremented)
               (if (and (> index 0)
                        (char= incremented #\a))
                   (recur password (- index 1)))
               password)))
    (recur password (- (length password) 1))))

(defun one-less (one other)
  (= (- (char-code other) (char-code one)) 1))

(defun three-letter-increase (password)
  (let (one two)
    (loop for p across password
       do (if (and one two
                   (one-less one two)
                   (one-less two p))
              (return-from three-letter-increase t)
              (progn 
                (setf one two)
                (setf two p))))
    nil))

(defun two-letter-pairs (password)
  (let (old-c pair)
    (loop for c across password
       do (progn 
            (if (and old-c (char= c old-c))
              (let ((new-pair (cons c old-c)))
                (if (and pair (not (equal pair new-pair))) 
                    (return-from two-letter-pairs t)
                    (setf pair new-pair))))
            (setf old-c c)))
    nil))

(defun doesnt-contain-banned (password)
  (loop for c across password
     do (if (or (char= c #\i)
                (char= c #\o)
                (char= c #\l))
            (return-from doesnt-contain-banned nil)))
  t)

(defun legal-password (password)
  (and (three-letter-increase password)
       (two-letter-pairs password)
       (doesnt-contain-banned password)))

(defun find-next-legal-password (password)
  (labels ((recur (password)
             (if (legal-password password)
                 password
                 (recur (increment-password password)))))
    (recur (increment-password password))))

(defun solution-part-1 ()
  (coerce (find-next-legal-password
           (map 'vector (lambda (c) c) "hxbxwxba")) 'string))

(defun solution-part-2 ()
  (coerce (find-next-legal-password 
           (find-next-legal-password (map 'vector (lambda (c) c) "hxbxwxba"))) 'string))
