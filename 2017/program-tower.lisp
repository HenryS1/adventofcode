(ql:quickload 'cl-ppcre)

(defun set-parent (parent children graph)
  (loop for child in children 
     do (setf (gethash child graph) parent)))

(defun set-children (parent weight children graph)
  (setf (gethash parent graph) (list weight children)))

(defun parse-input (str)
  (let ((splt (cl-ppcre:split "[^\\w]+" str)))
    (let ((parent (car splt))
          (weight (if (cadr splt) (parse-integer (cadr splt)) nil))
          (children (cddr splt)))
      (values parent weight children))))

(defun process-lines (filename callback)
  (with-open-file (f filename)
    (when f
      (loop for line = (read-line f nil nil)
         while line
         do (funcall callback line)))))

(defun build-upside-down (filename)
  (let ((upside-down (make-hash-table :test 'equal)))
    (process-lines filename (lambda (line)
                     (multiple-value-bind (parent weight children) (parse-input line)
                       (declare (ignorable weight))
                       (set-parent parent children upside-down))))
    upside-down))

(defun build-tree (filename)
  (let ((tree (make-hash-table :test 'equal)))
    (process-lines filename (lambda (line)
                     (multiple-value-bind (parent weight children) (parse-input line)
                       (set-children parent weight children tree))))
    tree))

(defun subtree-weight (node tree sub-weights)
  (if (gethash node sub-weights)
      (gethash node sub-weights)
      (destructuring-bind (weight children) (gethash node tree)
        (let ((sub-total 0))
          (loop for child in children 
             do (incf sub-total (subtree-weight child tree sub-weights)))
          (+ sub-total weight)))))

(defun balanced-children (node tree sub-weights)
  (let (current-weight)
    (destructuring-bind (weight children) (gethash node tree)
      (declare (ignorable weight))
      (loop for child in children
         do (let ((sub-weight (subtree-weight child tree sub-weights)))
              (if (and current-weight (/= sub-weight current-weight))
                (return-from balanced-children nil)
                (setf current-weight sub-weight)))))
    t))

(defun balanced-parent (node tree upside-down sub-weights)
  (let ((parent (gethash node upside-down)))
    (or (not parent) 
        (balanced-children parent tree sub-weights))))

(defun odd-one-out (node tree upside-down sub-weights)
  (let ((parent (gethash node upside-down)))
    (destructuring-bind (weight children) (gethash parent tree)
      (declare (ignorable weight))
      (let ((equal-count 0)
            (sub-weight (subtree-weight node tree sub-weights)))
        (loop for sibling in children
           when (= (subtree-weight sibling tree sub-weights)
                   sub-weight)
           do (incf equal-count))
        (= equal-count 1)))))

(defun has-wrong-weight (node tree upside-down sub-weights)
  (and (not (balanced-parent node tree upside-down sub-weights))
       (balanced-children node tree sub-weights)
       (odd-one-out node tree upside-down sub-weights)))

(defun find-correct-weight (node tree upside-down sub-weights)
  (let ((parent (gethash node upside-down))
        (sub-weight (subtree-weight node tree sub-weights)))
    (destructuring-bind (weight children) (gethash parent tree)
      (loop for sibling in children
           when (/= (subtree-weight sibling tree sub-weights) sub-weight)
           do (progn 
                (return-from find-correct-weight 
                  (+ (car (gethash node tree))
                     (- (subtree-weight sibling tree sub-weights) sub-weight)))))
      weight)))

(defun find-root (node upside-down)
  (if (gethash node upside-down)
      (find-root (gethash node upside-down) upside-down)
      node))

(defun traverse-tree (node tree callback)
  (progn 
    (funcall callback node)
    (destructuring-bind (weight children) (gethash node tree)
      (declare (ignorable weight))
      (loop for child in children
         do (traverse-tree child tree callback)))))

(defun find-wrong-weight (tree upside-down)
  (let ((sub-weights (make-hash-table :test 'equal))
        (root (find-top upside-down))
        wrong-node)
    (traverse-tree root tree (lambda (node)
                               (if (has-wrong-weight node tree upside-down sub-weights)
                                   (setf wrong-node node))))
    (values wrong-node (find-correct-weight wrong-node tree upside-down sub-weights))))

(defun find-top (upside-down)
  (loop for k being the hash-keys of upside-down
     when k
     do (return-from find-top (find-root k upside-down))))

(defun solution-part-1 ()
  (find-top (build-upside-down "program-tower-input.txt")))

(defun wrong-weight-solver (filename)
  (let ((tree (build-tree filename))
        (upside-down (build-upside-down filename)))
    (find-wrong-weight tree upside-down)))

(defun test-tree () 
  (build-tree "program-tower-test-input.txt"))

(defun test-upside-down ()
  (build-upside-down "program-tower-test-input.txt"))

(defun test-part-2 ()
  (wrong-weight-solver "program-tower-test-input.txt"))

(defun solution-part-2 ()
  (wrong-weight-solver "program-tower-input.txt"))
