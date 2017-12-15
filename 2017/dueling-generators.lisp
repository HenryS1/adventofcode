(let ((modulus 2147483647))
  (defun gen-next (factor current)
    (mod (* current factor) modulus)))

(let ((a-factor 16807))
  (defun gen-next-a (current)
    (let ((next (gen-next a-factor current)))
      (if (= (mod next 4) 0)
          next
          (gen-next-a next)))))

(let ((b-factor 48271))
  (defun gen-next-b (current)
    (let ((next (gen-next b-factor current)))
      (if (= (mod next 8) 0)
          next
          (gen-next-b next)))))

(let ((modulus (expt 2 16)))
  (defun equal-lower-16 (one other)
    (= (mod one modulus) (mod other modulus))))

(defun count-agreements (a-seed b-seed iter-limit)
  (labels ((recur (a-current b-current total iters)
               (if (= iters iter-limit)
                   total
                   (if (equal-lower-16 a-current b-current)
                       (recur (gen-next-a a-current)
                              (gen-next-b b-current)
                              (+ total 1)
                              (+ iters 1))
                       (recur (gen-next-a a-current)
                              (gen-next-b b-current)
                              total 
                              (+ iters 1))))))
    (recur (gen-next-a a-seed) (gen-next-b b-seed) 0 0)))

(defun test-part-1 ()
  (count-agreements 65 8921 40000000))

(defun solution-part-1 ()
  (count-agreements 512 191 40000000))

(defun test-part-2 ()
  (count-agreements 65 8921 5000000))

(defun solution-part-2 ()
  (count-agreements 512 191 5000000))
