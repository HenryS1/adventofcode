(defun memoize (f)
  (let ((cache (make-hash-table :test 'equal))
        (original-fun (symbol-function f)))
    (setf (symbol-function f)
          (lambda (&rest args) 
            (multiple-value-bind (val found) (gethash args cache)
                (if found
                    val
                    (let ((result (apply original-fun args)))
                      (setf (gethash args cache) result)
                      result)))))))

(defmacro defmemoized (name &rest body)
  `(progn (defun ,name ,@body)
          (memoize ',name)))

(defun first-prime-factor (n)
  (cond ((= (mod n 2) 0) 2)
        (t (loop for i = 3 then (+ i 2)
              until (or (= (mod n i) 0) 
                        (> i (ceiling (sqrt n))))
              finally (return (if (= (mod n i) 0)
                                  i
                                  n))))))

(defun divide-out (d n)
  (loop while (= (mod n d) 0) 
       for aggregate = d then (* d aggregate)
       do (setf n (/ n d))
       finally (return (cons n aggregate))))

(defmemoized factor-sum (n)
  (cond ((= n 1) 1)
        (t (let ((fpf (first-prime-factor n)))
               (destructuring-bind (divided . factor) (divide-out fpf n)
                 (+ (factor-sum (/ n fpf)) (* factor (factor-sum divided))))))))

(defun factors-to-minus (n)
  (loop for i from 1 to (1- (floor n 50))
     when (= (mod n i) 0)
     sum i))

(defun house-presents (n)
  (* (- (factor-sum n) (factors-to-minus n)) 11))

(defun find-house-number (presents)
  (loop for i = 1 then (1+ i)
     until (>= (factor-sum i) (/ presents 10))
     finally (return i)))

(defun find-house-number-strict (presents)
  (loop for i = (find-house-number presents) then (1+ i)
     until (>= (house-presents i) presents)
     finally (return i)))
