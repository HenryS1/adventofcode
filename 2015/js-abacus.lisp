(ql:quickload 'cl-ppcre)
(ql:quickload 'jsown)

(defun get-numbers (str)
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" str)))

(defun get-input ()
  (with-open-file (f "js-abacus-input.txt")
    (when f 
      (read-line f))))

(defun solution-part-1 ()
  (reduce #'+ (get-numbers (get-input))))

(defun parse-json (str)
  (jsown:parse str))

(defun is-object (json)
  (and (consp json) 
       (eq (car json) :obj)))

(defun contains-red-value (js-object)
  (progn (jsown:do-json-keys (k v)
           js-object
           (if (and (stringp v)
                    (string= v "red"))
               (return-from contains-red-value t)))
         nil))

(defun json-object-sum (js-object)
  (if (contains-red-value js-object)
      0
      (let ((total 0))
        (jsown:do-json-keys (k v)
          js-object
          (cond ((numberp v)
                 (incf total v))
                ((consp v)
                 (incf total (json-sum v)))))
        total)))

(defun json-array-sum (js-object)
  (let ((total 0))
    (loop for el in js-object
       do (cond ((numberp el)
                 (incf total el))
                ((consp el)
                 (incf total (json-sum el)))))
    total))

(defun json-sum (json)
  (if (is-object json)
      (json-object-sum json)
      (json-array-sum json)))

(defun solution-part-2 ()
  (let ((json (jsown:parse (get-input))))
    (json-sum json)))
