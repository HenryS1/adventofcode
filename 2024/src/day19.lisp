(defpackage :day19
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

(in-package :day19)

(defun parse-towels (line)
  (let ((without-comma (remove-if (lambda (c) (char= c #\,)) line))
        *read-eval*)
    (with-input-from-string (s without-comma)
      (loop for symbol = (read s nil nil)
            while symbol collect (string-downcase (symbol-name symbol))))))

(defun read-input (file)
  (with-open-file (f file)
    (let ((towels (parse-towels (read-line f))))
      (read-line f)
      (loop for line = (read-line f nil nil)
            while line collect line into patterns
            finally (return (cons towels patterns))))))

(defun ordered-already-seen-patterns (already-seen)
  (sort (hash-table-keys already-seen) (lambda (a b) (> (length a) (length b)))))

(defun prefix-matches (candidate pattern start-index end-index)
  (when (>= (- end-index start-index) (length candidate))
    (loop for one across candidate
          for i from start-index to (- end-index 1)
          when (char/= one (aref pattern i))
            do (return nil)
          finally (return t))))

(defun pattern-is-possible (towels pattern start-index end-index candidates impossible)
  (let ((key (make-array (- end-index start-index) :element-type 'character
                         :displaced-to pattern :displaced-index-offset start-index)))
    (and (not (gethash key impossible))
         (or (= start-index end-index)
             (loop for candidate in candidates
                   for i = (length candidate)
                   when (and (<= (length candidate) (- end-index start-index))
                             (prefix-matches candidate pattern start-index (+ i start-index))
                             (pattern-is-possible towels pattern (+ i start-index)
                                                  end-index candidates impossible))
                     do (return t)
                   finally (progn (setf (gethash key impossible) t)
                                  (return nil)))))))

(defun part1 ()
  (let* ((towels-and-patterns (read-input "day19input"))
         (towels (sort (car towels-and-patterns) (lambda (a b) (> (length a) (length b)))))
         (patterns (cdr towels-and-patterns))
         (impossible (make-hash-table :test 'equal)))
    (loop for pattern in patterns
          counting (pattern-is-possible 
                    towels pattern 0
                    (length pattern) 
                    towels
                    impossible))))

(defun ways-to-make (towels pattern start-index end-index impossible ways-to-make)
  (let* ((key (make-array (- end-index start-index) :element-type 'character
                         :displaced-to pattern :displaced-index-offset start-index))
         (ways 0)
         (lookup (gethash key ways-to-make)))
    (or lookup
        (and (not (gethash key impossible))
             (or (and (= start-index end-index) 1)
                 (loop for candidate in towels
                       for i = (length candidate)
                       for new-ways = (and (<= (length candidate) (- end-index start-index))
                                           (prefix-matches candidate pattern start-index 
                                                           (+ i start-index))
                                           (ways-to-make towels pattern (+ i start-index)
                                                         end-index impossible ways-to-make))
                       when new-ways
                         do (incf ways new-ways)
                       finally (if (= ways 0)
                                   (setf (gethash key impossible) t)
                                   (setf (gethash key ways-to-make) ways))
                               (return ways)))))))

(defun part2 ()
  (let* ((towels-and-patterns (read-input "day19input"))
         (towels (sort (car towels-and-patterns) (lambda (a b) (> (length a) (length b)))))
         (patterns (cdr towels-and-patterns))
         (impossible (make-hash-table :test 'equal))
         (ways-to-make (make-hash-table :test 'equal)))
    (loop for pattern in patterns
          summing (ways-to-make 
                    towels pattern 0
                    (length pattern) 
                    impossible
                    ways-to-make))))
