(ql:quickload :cl-ppcre)

(defun read-patch (line)
  (multiple-value-bind (m rs) (cl-ppcre:scan-to-strings
                               "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" line)
    (declare (ignore m))
    (map 'list #'parse-integer rs)))

(defun collect-patches (lines) (mapcar #'read-patch lines))

(defun read-lines ()
  (with-open-file (f "no-matter-how-you-slice-it-input.txt")
    (when f 
      (loop for line = (read-line f nil nil)
         while line
         collect line))))

(defun add-patch (squares patch)
  (destructuring-bind (id row-start col-start rows cols) patch
    (declare (ignore id))
    (loop for row from row-start to (+ row-start rows -1)
       do (loop for col from col-start to (+ col-start cols -1)
             do (when (not (gethash (cons row col) squares))
                  (setf (gethash (cons row col) squares) 0))
               (incf (gethash (cons row col) squares))))))

(defun count-overlapping-claims (squares)
  (let ((total 0))
    (loop for count being the hash-values of squares
       when (> count 1)
       do (incf total))
    total))

(defun find-overlap-count (lines)
  (let ((patches (collect-patches lines))
        (squares (make-hash-table :test 'equal)))
    (loop for patch in patches 
       do (add-patch squares patch))
    (count-overlapping-claims squares)))

(defun patch-has-overlap (squares patch)
  (destructuring-bind (id row-start col-start rows cols) patch
    (declare (ignore id))
    (loop for row from row-start to (+ row-start rows -1)
       do (loop for col from col-start to (+ col-start cols -1)
             when (> (gethash (cons row col) squares) 1)
             do (return-from patch-has-overlap t)))
    nil))

(defun find-overlap-free-patch (lines)
  (let ((patches (collect-patches lines))
        (squares (make-hash-table :test 'equal)))
    (loop for patch in patches
       do (add-patch squares patch))
    (car (remove-if (lambda (p) (patch-has-overlap squares p)) patches))))

(defun solution-part-1 ()
  (find-overlap-count (read-lines)))

(defun solution-part-2 ()
  (find-overlap-free-patch (read-lines)))
