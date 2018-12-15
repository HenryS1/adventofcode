(load "../2018/utilities.lisp")
(ql:quickload :cl-ppcre)

(defun parse-input (line)
  (multiple-value-bind (m rs)
      (cl-ppcre:scan-to-strings "([a-zA-Z-]+)(\\d+)\\[(\\w+)\\]" line)
    (declare (ignore m))
    (cons (cons (remove-if (lambda (c) (char= c #\-)) (aref rs 0))
          (aref rs 2))
          (parse-integer (aref rs 1)))))

(defun parse-ids () 
  (read-lines "security-through-obscurity-input.txt" #'parse-input))

(defun frequency-compare (frequencies)
  (lambda (a b) (> (gethash a frequencies) (gethash b frequencies))))

(defun is-valid-room (name checksum)
  (let ((character-frequencies (frequencies name)))
    (let* ((cs (sort (hash-keys character-frequencies) #'char<))
           (expected-checksum-chars (take 5 (stable-sort
                                             cs (frequency-compare character-frequencies)))))
      (string= (map 'string #'identity expected-checksum-chars)
               checksum))))

(defun sum-valid-ids (ids)
  (reduce #'+ (mapcar #'cdr (remove-if-not
                             (lambda (id) (is-valid-room (caar id) (cdar id)))
                             ids))))

(defun solution-part-1 ()
  (sum-valid-ids (parse-ids)))

(defun char-index (c)
  (- (char-code c) 97))


(defparameter *alphabet* "abcdefghijklmnopqrstuvwxyz")

(defun shift-letter (num-forward)
  (lambda (c) 
    (case c 
      (#\- #\space)
      (otherwise (aref *alphabet* (mod (+ (char-index c) num-forward) 26))))))

(defun decrypt (name sector-id)
  (map 'string (shift-letter sector-id) name))

(defun find-north-pole-objects (ids)
  (loop for id in ids
     when (is-valid-room (caar id) (cdar id))
     do (let ((decrypted (decrypt (caar id) (cdr id))))
          (when (search "north" decrypted)
            (return-from find-north-pole-objects (cdr id))))))

(defun solution-part-2 ()
  (find-north-pole-objects (parse-ids)))
