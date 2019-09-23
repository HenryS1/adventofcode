(ql:quickload :ironclad)

(defun hash (str)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence :md5 
                             (ironclad:ascii-string-to-byte-array str))))

(defun find-hash (salt number)
  (hash (concatenate 'string salt (write-to-string number))))

(defparameter *salt* "jlmsuwbz")

(defun find-triple (s)
  (loop for i from 0 to (- (length s) 3)
     for a = (aref s i)
     for b = (aref s (+ i 1))
     for c = (aref s (+ i 2))
     when (char= a b c)
     collect (subseq s i (+ i 3))
     until (char= a b c)))

(defun find-pents (s)
  (loop for i from 0 to (- (length s) 5)
     for a = (aref s i)
     for b = (aref s (+ i 1))
     for c = (aref s (+ i 2))
     for d = (aref s (+ i 3))
     for e = (aref s (+ i 4))
     when (char= a b c d e)
     collect (subseq s i (+ i 5))))

(defun find-nth-key (salt n hash-fun)
  (let ((seen-pents (make-hash-table :test 'equal))
        (seen-triples (make-hash-table :test 'equal))
        (key-indices (make-hash-table))
        (indices-left 1000))
    (loop for i = 0 then (+ i 1)
       for hash = (funcall hash-fun salt i)
       for triples = (find-triple hash)
       for pents = (find-pents hash)
       while (> indices-left 0)
       when (>= (hash-table-count key-indices) n)
       do (decf indices-left)
       when triples
       do (mapc (lambda (triple) (push i (gethash triple seen-triples))) triples)
       when pents 
       do (mapc (lambda (pent) (push i (gethash pent seen-pents))) pents)
         (mapc (lambda (pent) 
                 (mapc (lambda (ind)
                           (when  (< 0 (- i ind) 1000)
                             (setf (gethash ind key-indices) t)))
                       (gethash (subseq pent 2) seen-triples))) pents)
       finally (return 
                 (nth (- n 1) (sort (loop for index being the hash-keys of key-indices
                                       collect index) #'<))))))

(defun answer1 ()
  (find-nth-key *salt* 64 #'find-hash))

(defun key-stretched-hash (salt i)
  (loop for iter from 1 to 2017
     for hsh = (find-hash salt i) then (hash hsh)
     finally (return hsh)))

(defun answer2 ()
  (find-nth-key *salt* 64 #'key-stretched-hash))
