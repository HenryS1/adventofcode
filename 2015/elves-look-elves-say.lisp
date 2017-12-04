(defun count-together (nums)
  (let ((current (car nums))
        (count 0)
        (rest nums))
    (labels ((recur (ns)
               (if (and ns (= (car ns) current))
                   (progn 
                     (incf count)
                     (setf rest (cdr ns))
                     (recur (cdr ns))))))
      (recur nums))
    (values count current rest)))

(defun look-and-say (nums)
  (labels ((recur (nums acc)
             (if (null nums)
                 (reverse acc)
                 (multiple-value-bind (count current rest) (count-together nums)
                   (recur rest (cons current (cons count acc)))))))
    (recur nums nil)))

(defun look-and-say-repeat (times nums)
  (if (= times 0)
      nums
      (look-and-say-repeat (- times 1) (look-and-say nums))))

(defun parse-string (num-str)
  (map 'list #'digit-char-p num-str))

(defun solution-part-1 ()
  (length (look-and-say-repeat 40 (parse-string "3113322113"))))

(defun solution-part-2 ()
  (length (look-and-say-repeat 50 (parse-string "3113322113"))))
