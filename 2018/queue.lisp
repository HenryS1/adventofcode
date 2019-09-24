(defun make-queue (&rest init)
  (list init (last init)))

(defun enqueue (e q)
  (if (null (car q))
      (let ((l (list e)))
        (setf (car q) l
              (cadr q) l))
      (let ((new-tl (list e)))
        (setf (cdr (cadr q)) new-tl
              (cadr q) new-tl)))
  q)

(defun peek (q)
  (caar q))

(defun poll (q)
  (pop (car q)))

(defun non-empty (q)
  (car q))

(defun empty (q)
  (null (car q)))

(defun size (q)
  (length (car q)))

(defun next-view (q)
  (car q))
