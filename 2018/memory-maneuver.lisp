(load "utilities.lisp")
(ql:quickload :cl-ppcre)

(defun read-tree (start-position node-defs)
  (let ((num-children (aref node-defs start-position))
        (num-metadata (aref node-defs (1+ start-position)))
        metadata
        children)
    (incf start-position 2)
    (loop for i from 1 to num-children
       do (multiple-value-bind (child next-position) (read-tree start-position node-defs)
            (setf start-position next-position)
            (push child children)))
    (loop for i from 1 to num-metadata
       do (push (aref node-defs start-position) metadata)
         (incf start-position))
    (values (cons (map 'vector #'identity (reverse children))
                  (map 'vector #'identity (reverse metadata)))
            start-position)))

(defun sum-metadata (node)
  (+ (reduce #'+ (cdr node))
     (reduce #'+ (map 'vector #'sum-metadata (car node)))))

(defun read-input ()
  (car (read-lines "memory-maneuver-input.txt" 
                   (lambda (line) (map 'vector #'parse-integer (cl-ppcre:split "\\s+" line))))))

(defun solution-part-1 ()
  (let ((root-node (read-tree 0 (read-input))))
    (sum-metadata root-node)))

(defun node-value (node)
  (let ((children (car node))
        (metadata (cdr node))
        (sm 0))
    (loop for i across metadata
       when (<= i (length children))
       do (incf sm (node-value (aref children (1- i)))))
    (when (= (length children) 0)
      (incf sm (reduce #'+ metadata)))
    sm))

(defun test-1 ()
  (sum-metadata (read-tree 0 (vector 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))))

(defun test-2 ()
  (node-value (read-tree 0 (vector 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))))

(defun solution-part-2 ()
  (let ((root-node (read-tree 0 (read-input))))
    (node-value root-node)))
