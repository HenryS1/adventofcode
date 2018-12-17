(ql:quickload :ironclad)
(ql:quickload :flexi-streams)

(defun hash-str (str)
  (let ((digester (ironclad:make-digest :md5)))
    (ironclad:byte-array-to-hex-string 
     (ironclad:digest-sequence digester (flexi-streams:string-to-octets str)))))

(defun contains-password-character (hash)
  (string= (subseq hash 0 5) "00000"))

(defun find-password (key)
  (loop with password = nil
     while (< (length password) 8)
     for i = 1 then (1+ i)
     for hash = (hash-str (concatenate 'string key (format nil "~a" i)))
     when (contains-password-character hash)
     do (format t "FOUND~%")
       (push (aref hash 5) password)
     finally (return (coerce (reverse password) 'string))))

(defun solution-part-1 ()
  (find-password "wtnhxymk"))

(defun get-password-character (hash)
  (aref hash 6))

(defun has-valid-index (hash)
  (let ((d (digit-char-p (aref hash 5))))
    (and d (<= 0 d 7))))

(defun get-character-index (hash)
  (digit-char-p (aref hash 5)))

(defun find-password-using-index (key)
  (loop with password = (map 'string #'identity "12345678")
     with found = 0
     with seen = (make-hash-table)
     while (< found 8)
     for i = 1 then (1+ i)
     for hash = (hash-str (concatenate 'string key (format nil "~a" i)))
     when (and (contains-password-character hash) (has-valid-index hash))
     do (let ((index (get-character-index hash)))
         (when (not (gethash index seen))
           (format t "FOUND~%")
           (setf (gethash index seen) t)
           (setf (aref password index) (get-password-character hash))
           (incf found)))
     finally (return (coerce password 'string))))

(defun solution-part-2 ()
  (find-password-using-index "wtnhxymk"))
