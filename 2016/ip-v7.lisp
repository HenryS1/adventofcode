(ql:quickload :cl-ppcre)

(defparameter *input-file* "ip-v7-input.txt")

(defun parse-line (line)
  (loop with groups = (cl-ppcre:split "[\\[\\]]" line)
     for group in groups
     for i = (if (char= (aref line 0) #\[) 1 0) then (mod (1+ i) 2)
     when (= i 0)
     collect group into outside
     when (= i 1)
     collect group into inside
     finally (return (values outside inside))))

(defun supports-tls (line)
  (multiple-value-bind (outside inside) (parse-line line)
    (and (some #'has-abba outside)
         (every (lambda (str) (not (has-abba str))) inside))))

(defun read-input (filename) 
  (with-open-file (f filename)
    (when f
      (loop for line = (read-line f nil nil)
         while line 
         collect line))))

(defun is-abba (str)
  (and (char= (aref str 0) (aref str 3))
       (char= (aref str 1) (aref str 2))
       (char/= (aref str 0) (aref str 1))))

(defun has-abba (str)
  (loop for i from 0 to (- (length str) 4)
     when (is-abba (subseq str i (+ i 4)))
     return t
     finally (return nil)))

(defun is-aba (str)
  (and (char= (aref str 0) (aref str 2))
       (char/= (aref str 0) (aref str 1))))

(defun collect-str-abas (str abas)
  (loop for i from 0 to (- (length str) 3)
     when (is-aba (subseq str i (+ i 3)))
     do (setf (gethash (subseq str i (+ i 3)) abas) t)))

(defun collect-abas (group)
  (loop with abas = (make-hash-table :test 'equal)
     for str in group
     do (collect-str-abas str abas)
     finally (return abas)))

(defun supports-ssl (line)
  (multiple-value-bind (outside inside) (parse-line line)
    (loop with abas = (collect-abas outside)
       with babs = (collect-abas inside)
       for aba being the hash-keys of abas
       when (gethash (coerce (vector (aref aba 1) (aref aba 0) (aref aba 1)) 'string) babs)
       return t
       finally (return nil))))

(defun solution-part-1 ()
  (count-if #'supports-tls (read-input *input-file*)))

(defun solution-part-2 ()
  (count-if #'supports-ssl (read-input *input-file*)))
