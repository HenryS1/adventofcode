(defpackage :day21
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :queue
   :fixnum-pq
   :anaphora 
   :metabang-bind)
  (:export 
   :calibration-value
   :total-calibration-value
   :digits-in-line
   :all-calibration-values))

(in-package :day21)

(defun read-input (file)
  (with-open-file (f file)
    (loop for line = (read-line f nil nil)
          while line collect line)))

(defun pack (row-diff col-diff)
  (logior (ash row-diff 32) col-diff))

(defun row-diff (packed)
  (ash packed -32))

(defun col-diff (packed) 
  (logand packed (- (ash 1 32) 1)))

(defun numeric-pad-coordinate (c)
  (case c
    (#\7 #c(0 0))
    (#\8 #c(0 1))
    (#\9 #c(0 2))
    (#\4 #c(1 0))
    (#\5 #c(1 1))
    (#\6 #c(1 2))
    (#\1 #c(2 0))
    (#\2 #c(2 1))
    (#\3 #c(2 2))
    (#\0 #c(3 1))
    (#\A #c(3 2))))

(defun shortest-path-on-numeric (sequence)
  (loop for start = (numeric-pad-coordinate #\A) then (numeric-pad-coordinate end)
        for end across sequence
        for offset = (- (numeric-pad-coordinate end) start)
        collect (numeric-offset-to-arrows offset start)))

(defun row-diff-arrows (row-diff)
  (cond ((> row-diff 0) (cons (abs row-diff) 'v))
        ((< row-diff 0) (cons (abs row-diff) '^))
        (t nil)))

(defun col-diff-arrows (col-diff)
  (cond ((> col-diff 0) (cons (abs col-diff) '>))
        ((< col-diff 0) (cons (abs col-diff) '<))
        (t nil)))

(defun numeric-offset-to-arrows (offset start)
  (let ((row-diff (realpart offset))
        (col-diff (imagpart offset)))
    (cond ((= row-diff 0)
           (list (list (col-diff-arrows col-diff))))
          ((= col-diff 0)
           (list (list (row-diff-arrows row-diff))))
          ((and (= (realpart start) 3)
                (= (+ (imagpart offset) (imagpart start)) 0))
           (list (list (row-diff-arrows row-diff) (col-diff-arrows col-diff))))
          ((and (= (imagpart start) 0)
                (= (+ (realpart offset) (realpart start) 3)))
           (list (list (col-diff-arrows col-diff) (row-diff-arrows row-diff))))
          (t (list (list (row-diff-arrows row-diff) (col-diff-arrows col-diff))
                   (list (col-diff-arrows col-diff) (row-diff-arrows row-diff)))))))

(defun arrows-between (one other)
  (case one 
    (< (case other
         (< nil)
         (> '((2 . >)))
         (^ '((1 . >) (1 . ^)))
         (v '((1 . >)))
         (A '((2 . >) (1 . ^)))))
    (> (case other
         (< '((2 . <)))
         (> nil)
         (^ '((1 . ^) (1 . <)))
         (v '((1 . <)))
         (A '((1 . ^)))))
    (^ (case other
         (< '((1 . v) (1 . <)))
         (> '((1 . >) (1 . v)))
         (v '((1 . v)))
         (^ nil)
         (A '((1 . >)))))
    (v (case other 
         (< '((1 . <)))
         (> '((1 . >)))
         (v nil)
         (^ '((1 . ^)))
         (A '((1 . >) (1 . ^)))))
    (A (case other 
         (< '((1 . v) (2 . <)))
         (> '((1 . v)))
         (^ '((1 . <)))
         (v '((1 . v) (1 . <)))
         (A nil)))))

(defun arrows-to-arrows (arrows)
  (loop for start = 'A then (cdr arrow)
        for arrow in arrows
        for between = (arrows-between start (cdr arrow))
        appending between
        collecting (cons (car arrow) 'A)))
 
(defun complexity (code)
  (let* ((combinations (all-combinations (shortest-path-on-numeric code))))
    (reduce #'min 
            (mapcar (lambda (arrows) (* (reduce #'+ (mapcar #'car (arrows-to-arrows
                                                                   (arrows-to-arrows arrows))))
                                        (parse-integer code :junk-allowed t)))
                    combinations))))

(defun format-arrows (arrows)
  (let ((result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
   (with-output-to-string (s result)
     (loop for arrow in arrows
           do (loop for i from 1 to (car arrow)
                    do (format s "~a" (cdr arrow))))
     result)))

(defun all-combinations (numeric-path-options)
  (if (null numeric-path-options)
      (list nil)
      (let* ((next (car numeric-path-options))
             (results (all-combinations (cdr numeric-path-options)))
             (a (cons 1 'A)))
        (if (cadr next)
            (append (mapcar (lambda (rest) (append (car next) (cons a rest)))
                            results)
                    (mapcar (lambda (rest) (append (cadr next) (cons a rest)))
                            results))
            (mapcar (lambda (rest) (append (car next) (cons a rest))) results)))))

(defun part1 ()
  (let* ((codes (read-input "day21input")))
    (reduce #'+ (mapcar #'complexity codes))))

(defun char-arrows-between (one other)
  (case one 
    (#\< (case other
         (#\< "A")
         (#\> ">>A")
         (#\^ ">^A")
         (#\V ">A")
         (#\A ">>^A")))
    (#\> (case other
         (#\< "<<A")
         (#\> "A")
         (#\^ "^<A")
         (#\V "<A")
         (#\A "^A")))
    (#\^ (case other
         (#\< "V<A")
         (#\> ">VA")
         (#\V "VA")
         (#\^ "A")
         (#\A ">A")))
    (#\V (case other 
         (#\< "<A")
         (#\> ">A")
         (#\V "A")
         (#\^ "^A")
         (#\A ">^A")))
    (#\A (case other 
         (#\< "V<<A")
         (#\> "VA")
         (#\^ "<A")
         (#\V "V<A")
         (#\A "A")))))

(defun char-arrows-to-arrows (arrows)
  (loop for start = #\A then arrow
        for arrow across arrows
        for between = (char-arrows-between start arrow)
        collect between))

(defun char-arrows-to-char-arrows (counts productions)
  (let ((next-counts (make-hash-table :test 'equal)))
    (loop for k being the hash-keys of counts using (hash-value v)
          for production = (or (gethash k productions)
                               (setf (gethash k productions) 
                                     (char-arrows-to-arrows k)))
          do (loop for p in production
                   do (setf (gethash p next-counts)
                            (+ (gethash p next-counts 0) v)))
          finally (return next-counts))))

(defun split-on-a (moves)
  (let (result)
    (loop with start = 0
          for end from 1 
          for c across moves
          when (char= c #\A)
            do (push (subseq moves start end) result)
               (setf start end)
          finally (return result))))

(defun total-required-buttons (counts)
  (loop for k being the hash-keys of counts using (hash-value v)
        summing (* v (length k))))

(defun iterate-arrows (arrows n)
  (let ((productions (make-hash-table :test 'equal))
        (initial-counts (make-hash-table :test 'equal))
        (initial-groups (split-on-a (format-arrows arrows))))
    (format t "INITIAL ARROWS ~a~%" initial-groups)
    (loop for group in initial-groups do (setf (gethash group initial-counts)
                                               (+ (gethash group initial-counts 0) 1)))
    (format t "COUNTS ~a~%" (total-required-buttons initial-counts))
    (loop for counts = initial-counts then (char-arrows-to-char-arrows counts productions)
          for i from 1 to n
          do (format t "COUNTS ITER ~a~%" (total-required-buttons counts))
          finally (return (progn (format t "RESULT ~a~%" (total-required-buttons counts))
                                 (total-required-buttons counts))))))

(defun repeated-complexity (code n)
  (let ((combinations (all-combinations (shortest-path-on-numeric code))))
    (reduce #'min (mapcar (lambda (combination) (* (iterate-arrows combination n) 1
                                                   (parse-integer code :junk-allowed t)))
                          combinations))))

(defun part2 ()
  (let* ((codes (read-input "day21input")))
    (reduce #'+ (mapcar (lambda (code) (repeated-complexity code 25)) codes))))
