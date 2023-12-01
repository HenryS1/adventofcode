(defpackage :day7
  (:use :cl :trivia :iterate :pears :alexandria :anaphora :metabang-bind))

(in-package :day7)

(neat-lambda:enable-lambda-syntax)

(defun rest-of-line () (many #l(not (newlinep %c))))

(defun parse-command ()
  (orp (sequential (_ (seq "$ cd ")) (dirname (rest-of-line)) (list 'cd dirname))
       (sequential (_ (seq "$ ls")) (list 'ls))
       (sequential (_ (seq "dir ")) (dirname (rest-of-line)) (list 'dir dirname))
       (sequential (size *positive-int*) (_ (char1 #\space)) (filename (rest-of-line)) 
                   (list 'file size filename))))

(defun parse-lines ()
  (parse-file "day7.input" (sep-by (parse-command) (char1 #\newline))))

(defun map-filesystem (commands)
  (let ((filesystem (make-hash-table :test 'equal)))
    (setf (gethash 'name filesystem) "/")
    (labels ((rec (commands current-dir)
               (if (null commands)
                   filesystem
                   (match (car commands)
                     ((list 'cd name)
                      (cond ((string= name "/")
                             (rec (cdr commands) filesystem))
                            ((string= name "..")
                             (rec (cdr commands) (gethash 'one-up current-dir)))
                            (t (let ((next-dir (gethash name current-dir 
                                                        (make-hash-table :test 'equal))))
                                 (setf (gethash 'name next-dir) name)
                                 (setf (gethash name current-dir) next-dir)
                                 (setf (gethash 'one-up next-dir) current-dir)
                                 (rec (cdr commands) next-dir)))))
                     ((list 'ls)
                      (rec (cdr commands) current-dir))
                     ((list 'dir _)
                      (rec (cdr commands) current-dir))
                     ((list 'file size name)
                      (setf (gethash name current-dir) size)
                      (rec (cdr commands) current-dir))))))
      (rec commands filesystem))))

(defun compute-size (directory)
  (let ((size (iter (for (k v) in-hashtable directory)
                (reducing (cond ((symbolp k) 0)
                                ((numberp v) v)
                                (t (compute-size v))) by #'+))))
    (setf (gethash 'size directory) size)
    size))

(defun find-small-directories (directory)
  (let ((subdirs (iter (for (k v) in-hashtable directory)
                   (when (and (not (or (numberp v) (symbolp k))))
                     (appending (find-small-directories v))))))
    (if (<= (gethash 'size directory) 100000)
        (progn 
          (cons directory subdirs))
        subdirs)))

(defun part1 ()
  (let ((filesystem (map-filesystem (parse-lines))))
    (compute-size filesystem)
    (reduce #'+ (mapcar (lambda (dir) (gethash 'size dir)) (find-small-directories filesystem)))))

(defun eligible-directories-to-delete (filesystem)
  (let ((required (- 30000000 (- 70000000 (gethash 'size filesystem)))))
    (labels ((rec (current-dir)
               (let ((subdirs (iter (for (k v) in-hashtable current-dir)
                                (when (not (or (numberp v) (symbolp k)))
                                (appending (rec v))))))
                 (if (>= (gethash 'size current-dir) required)
                     (cons current-dir subdirs)
                     subdirs))))
      (rec filesystem))))

(defun part2 ()
  (let ((filesystem (map-filesystem (parse-lines))))
    (compute-size filesystem)
    (reduce #'min (mapcar (lambda (dir) (gethash 'size dir)) 
             (eligible-directories-to-delete filesystem)))))
