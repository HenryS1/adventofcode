(ql:quickload 'cl-ppcre)

;; children: 3
;; cats: 7
;; samoyeds: 2
;; pomeranians: 3
;; akitas: 0
;; vizslas: 0
;; goldfish: 5
;; trees: 3
;; cars: 2
;; perfumes: 1

(defun read-attributes (description)
  (let* ((cleaned (remove-if (lambda (c) (or (char= c #\:) (char= c #\,))) description))
         (split (cl-ppcre:split "\\s+" cleaned))
         (sue (make-hash-table :test 'equal)))
    (loop for l = split then (cddr l)
       while l
       do (setf (gethash (car l) sue) 
                (parse-integer (cadr l))))
    
    sue))

(defun compare-attributes (name v1 requirement)
  (cond ((or (string= name "cats") (string= name "trees"))
         (> v1 requirement))
        ((or (string= name "pomeranians") (string= name "goldfish"))
         (< v1 requirement))
        (t (= v1 requirement))))

(defun has-attributes (sue attributes)
  (loop for name being the hash-keys of attributes
     using (hash-value v)
     do (if (or (and (gethash name sue)
                     (/= (gethash name sue)
                         (gethash name attributes))))
            (return-from has-attributes nil)))
  t)

(defun has-attributes-ranged (sue attributes)
  (loop for name being the hash-keys of attributes
     using (hash-value v)
     do (if (or (and (gethash name sue)
                     (not (compare-attributes name
                                         (gethash name sue)
                                         (gethash name attributes)))))
            (return-from has-attributes-ranged nil)))
  t)

(defun find-sue-equal (sues attributes)
  (gethash "Sue" (find-if (lambda (sue) (has-attributes sue attributes)) sues)))

(defun find-sue-range (sues attributes)
  (gethash "Sue" (find-if (lambda (sue) (has-attributes-ranged sue attributes)) sues)))

(defun read-sues (file)
  (with-open-file (f file)
    (when f
      (loop for line = (read-line f nil nil)
         while line
         collect (make-sue line)))))

(defun solution-part-1 ()
  (let ((sues (read-sues "aunt-sue-input.txt"))
        (attributes (read-attributes "children: 3 cats: 7 samoyeds: 2 pomeranians: 3 akitas: 0 vizslas: 0 goldfish: 5 trees: 3 cars: 2 perfumes: 1")))
    (find-sue-equal sues attributes)))

(defun solution-part-2 ()
  (let ((sues (read-sues "aunt-sue-input.txt"))
        (attributes (read-attributes "children: 3 cats: 7 samoyeds: 2 pomeranians: 3 akitas: 0 vizslas: 0 goldfish: 5 trees: 3 cars: 2 perfumes: 1")))
    (find-sue-range sues attributes)))
