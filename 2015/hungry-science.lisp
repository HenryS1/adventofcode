(ql:quickload 'cl-ppcre)

(defmacro dbind (&rest body) 
  `(destructuring-bind ,@body))

(defmacro mvbind (&rest body)
  `(multiple-value-bind ,@body))

(defun get-ingredient-properties (description)
  (mvbind (m rs)
          (cl-ppcre:scan-to-strings
           "\\w+: capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)" 
           description)
          (declare (ignore m))
          (map 'list #'parse-integer rs)))

(defun ingredient-score (ingredients quantities)
  (apply #'* (mapcar (lambda (el) (max el 0))
                     (mapcar (lambda (l) (reduce #'+ l))
                             (list
                              (mapcar #'* quantities (mapcar #'car ingredients))
                              (mapcar #'* quantities (mapcar #'cadr ingredients))
                              (mapcar #'* quantities (mapcar #'caddr ingredients))
                              (mapcar #'* quantities (mapcar #'cadddr ingredients)))))))

(defun highest-scoring-cookie (ingredients)
  (let ((best 0))
    (loop for i from 0 to 100
       do (loop for j from 0 to (- 100 i)
             do (loop for k from 0 to (- 100 i j)
                   do (loop for l from 0 to (- 100 i j k)
                         do (let ((current (ingredient-score ingredients (list i j k l))))
                              (if (> current best)
                                  (setf best current)))))))
    best))

(defun calories (ingredients quantities)
  (reduce #'+ (mapcar #'* quantities (mapcar (lambda (i) (cadddr (cdr i))) ingredients))))

(defun highest-scoring-500-calorie-cookie (ingredients)
  (let ((best 0))
    (loop for i from 0 to 100
       do (loop for j from 0 to (- 100 i)
             do (loop for k from 0 to (- 100 i j)
                   do (loop for l from 0 to (- 100 i j k)
                         when (= (calories ingredients (list i j k l)) 500)
                         do (let ((current (ingredient-score ingredients (list i j k l))))
                              (if (> current best)
                                  (setf best current)))))))
    best))

(defun read-ingredients (file)
  (with-open-file (f file)
    (when f
      (loop for line = (read-line f nil nil)
         while line 
         collect (get-ingredient-properties line)))))

(defun solution-part-1 ()
  (highest-scoring-cookie (read-ingredients "hungry-science-input.txt")))

(defun solution-part-2 ()
  (highest-scoring-500-calorie-cookie (read-ingredients "hungry-science-input.txt")))
