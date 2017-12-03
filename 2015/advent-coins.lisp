(ql:quickload 'ironclad)
(ql:quickload 'flexi-streams)

(defparameter *key* "iwrupvqb")

(defun find-hash (num)
  (let* ((input (format nil "~a~a" *key* num)))
    (ironclad:digest-sequence :md5 (flexi-streams:string-to-octets input))))

(defun starts-with-five-zeros (hash)
  (and (= (aref hash 0) 0)
       (= (aref hash 1) 0)
       (<= (aref hash 2) 15)))

(defun starts-with-six-zeros (hash)
  (and (= (aref hash 0) 0)
       (= (aref hash 1) 0)
       (= (aref hash 2) 0)))

(defun find-lowest-number (check)
  (labels ((recur (i)
             (let ((hash (find-hash i)))
               (if (funcall check hash)
                   i
                   (recur (+ i 1))))))
    (recur 1)))

(defun solution-part-1 ()
  (find-lowest-number #'starts-with-five-zeros))

(defun solution-part-2 ()
  (find-lowest-number #'starts-with-six-zeros))
