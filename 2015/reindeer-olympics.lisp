(ql:quickload 'cl-ppcre)

(defun parse-input (line)
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings
       "(\\w+)\\s+.*\\s+(\\d+)\\s+.*\\s+(\\d+)\\s+.*\\s+(\\d+)\\s+.*" line)
    (declare (ignorable match))
    (values (aref registers 0) 
            (cons 'move (cons 0 (cons 0 (map 'list #'parse-integer (subseq registers 1))))))))

(defun adjust-location (stats)
  (destructuring-bind (action seconds distance speed move-seconds wait-seconds) stats
    (let ((new-distance (if (eq action 'move) (+ distance speed) distance)))
      (if (eq action 'move)
          (if (= (+ seconds 1) move-seconds)
              (cons 'wait (cons 0 (cons new-distance (cdddr stats))))
              (cons 'move (cons (+ seconds 1) (cons new-distance (cdddr stats)))))
          (if (= (+ seconds 1) wait-seconds)
              (cons 'move (cons 0 (cons new-distance (cdddr stats))))
              (cons 'wait (cons (+ seconds 1) (cons new-distance (cdddr stats)))))))))

(defun print-locations (names reindeer-stats)
  (loop for name in names
     do (format t "~a ~a~%" name (gethash name reindeer-stats))))

(defun print-points (points)
  (loop for name being the hash-keys of points using (hash-value v)
     do (format t "~a ~a~%" name v)))

(defun adjust-locations (names reindeer-stats)
;  (print-locations names reindeer-stats)
  (loop for name in names
     do (setf (gethash name reindeer-stats) 
              (adjust-location (gethash name reindeer-stats)))))

(defun find-winner (names reindeer-stats)
  (let (winners)
    (loop for name in names
       do (if (or (not winners)
                  (> (caddr (gethash name reindeer-stats)) (cdar winners)))
              (setf winners (list (cons name (caddr (gethash name reindeer-stats)))))
              (if (= (caddr (gethash name reindeer-stats)) (cdar winners))
                  (push (cons name (caddr (gethash name reindeer-stats))) winners))))
;    (format t "current winners are ~a~%" winners)
    winners))

(defun adjust-points (names reindeer-stats points)
  (let ((current-winners (find-winner names reindeer-stats)))
    (loop for winner in current-winners
       do (if (gethash (car winner) points)
        (incf (gethash (car winner) points))
        (setf (gethash (car winner) points) 1)))
;    (print-points points)
;    (format t "winner points ~a~%~%" (gethash (caar current-winners) points))
    ))

(defun race (seconds names reindeer-stats points)
  (loop for s from 1 to seconds
     do (progn 
;          (format t "seconds ~a~%" s)
          (adjust-locations names reindeer-stats)
          (adjust-points names reindeer-stats points))))

(defun process-lines (filename callback)
  (with-open-file (f filename)
    (when f
      (loop for line = (read-line f nil nil)
         while line
         do (funcall callback line)))))

(defun collect-reindeer (filename)
  (let (names
        (reindeer-stats (make-hash-table :test 'equal)))
    (process-lines filename (lambda (line)
                              (multiple-value-bind (name stats) (parse-input line)
                                (progn (push name names)
                                       (setf (gethash name reindeer-stats) stats)))))
    (values names reindeer-stats)))

(defun find-points-winner (points)
  (let (winner)
    (loop for k being the hash-keys of points using (hash-value v)
       when (or (not winner) (> v (cdr winner)))
       do (setf winner (cons k v)))
    winner))

(defun part-1-solver (seconds filename)
  (multiple-value-bind (names reindeer-stats) (collect-reindeer filename)
    (race seconds names reindeer-stats (make-hash-table :test 'equal))
    (find-winner names reindeer-stats)))

(defun part-2-solver (seconds filename)
  (multiple-value-bind (names reindeer-stats) (collect-reindeer filename)
    (let ((points (make-hash-table :test 'equal)))
      (race seconds names reindeer-stats points)
      (find-points-winner points))))

(defun test-part-1 ()
  (part-1-solver 1000 "reindeer-olympics-test-input"))

(defun solution-part-1 ()
  (part-1-solver 2503 "reindeer-olympics-input.txt"))

(defun test-part-2 ()
  (part-2-solver 1000 "reindeer-olympics-test-input"))

(defun solution-part-2 ()
  (part-2-solver 2503 "reindeer-olympics-input.txt"))
