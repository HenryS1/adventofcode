(ql:quickload :cl-ppcre)

(defun read-input ()
  (with-open-file (f "marble-mania-input.txt")
    (when f
      (remove-if-not #'identity (mapcar (lambda (str) (parse-integer str :junk-allowed t))
                                    (cl-ppcre:split "\\s+" (read-line f)))))))

(defmacro value (node)
  `(car ,node))

(defmacro next (node)
  `(cadr ,node))

(defmacro previous (node)
  `(caddr ,node))

(defun make-node (value)
  (let ((new-node (list value nil nil)))
    (setf (previous new-node) new-node)
    (setf (next new-node) new-node)
    new-node))

(defun add-node (first-node new-value)
  (let ((new-node (make-node new-value)))
    (setf (next new-node) first-node)
    (setf (previous new-node) (previous first-node))
    (setf (next (previous first-node)) new-node)
    (setf (previous first-node) new-node)
    new-node))

(defun go-clockwise (node n)
  (loop for i from 1 to n do (setf node (next node)))
  node)

(defun go-anti-clockwise (node n)
  (loop for i from 1 to n do (setf node (previous node)))
  node)

(defun remove-node (node)
  (setf (previous (next node)) (previous node))
  (setf (next (previous node)) (next node))
  (next node))

(defun next-move (current-marble n scores current-player)
  (if (= (mod n 23) 0)
      (let ((seven-back (go-anti-clockwise current-marble 7)))
        (if (not (gethash current-player scores))
            (setf (gethash current-player scores) n)
            (incf (gethash current-player scores) n))
        (incf (gethash current-player scores)
              (value seven-back))
        (remove-node seven-back))
      (let ((two-marbles-ahead (go-clockwise current-marble 2)))
        (add-node two-marbles-ahead n))))

(defun play-game (num-players last-marble)
  (let ((scores (make-hash-table)))
    (loop for i from 0 to last-marble
       for current-player = -1 then (mod (1+ current-player) num-players)
       for current-marble = (make-node 0) then (next-move current-marble i scores current-player))
    (let ((mx 0)
          best-player)
      (loop for player being the hash-keys of scores using (hash-value score)
         when (> score mx)
         do (setf mx score)
           (setf best-player player))
      (cons mx best-player))))

(defun solution-part-1 ()
  (destructuring-bind (num-players last-marble) (read-input)
    (play-game num-players last-marble)))

(defun solution-part-2 ()
  (destructuring-bind (num-players last-marble) (read-input)
    (play-game num-players (* 100 last-marble))))
