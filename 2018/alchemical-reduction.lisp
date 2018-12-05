(defun starts-with-oppposites (polymer)
  (and (char= (char-downcase (car polymer))
              (char-downcase (cadr polymer)))
       (or (lower-case-p (car polymer))
           (lower-case-p (cadr polymer)))
       (or (upper-case-p (car polymer))
           (upper-case-p (cadr polymer)))))

(defun reduce-polymer (polymer)
  (if (and (cdr polymer) (starts-with-oppposites polymer))
      (setf polymer (cddr polymer)))
  (loop for ps = polymer then (cdr ps)
     while (cddr ps)
     when (starts-with-oppposites (cdr ps))
     do (setf (cdr ps) (cdddr ps)))
  polymer)

(defun reduce-completely (polymer)
  (let ((previous-len (length polymer)))
    (loop for ps = (reduce-polymer polymer) then (reduce-polymer ps)
       while (< (length ps) previous-len)
       do (setf previous-len (length ps))
       finally (return ps))))

(defun read-input ()
  (with-open-file (f "alchemical-reduction-input.txt")
    (when f 
      (map 'list #'identity (read-line f nil nil)))))

(defun solution-part-1 ()
  (reduce-completely (read-input)))

(defun find-problematic-unit (polymer)
  (let ((len 999999999))
    (loop for c across "abcd"
       do (let* ((reduced (reduce-completely (remove-if (lambda (el) (char= (char-downcase el) c))
                                                        polymer)))
                 (new-len (length reduced)))
            (when (< new-len len)
              (setf len new-len))))
    len))

(defun solution-part-2 ()
  (find-problematic-unit (read-input)))
