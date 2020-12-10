(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :trivia)
  (ql:quickload :trivia.ppcre)
  (ql:quickload :cl-ppcre)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind)
  (ql:quickload :alexandria)
  (ql:quickload :cl-arrows)
  (load "../2018/queue.lisp")
  (load "../2018/priority-queue.lisp"))

(defpackage :day10
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day10)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input10" using #'read-line)
        (collect line)))

(defun joltage-differences (lines)
  (iter (with jolts = (cons 0 (sort (mapcar #'parse-integer lines) #'<)))
        (with diffs = (make-hash-table))
        (for j in jolts)
        (for jp previous j)
        (when jp 
          (setf (gethash (- j jp) diffs)
                (+ (gethash (- j jp) diffs 0) 1)))
        (finally (return (list (* (gethash 1 diffs) (+ 1 (gethash 3 diffs)))
                               (gethash 1 diffs) (+ 1 (gethash 3 diffs)))))))

(defun ways-to-join (nums)
  (let ((cache (make-hash-table)))
    (labels ((rec (i)
               (cond ((gethash i cache)
                      (gethash i cache))
                     ((= i 0) 1)
                     (t (let ((res (iter (for j from (- i 1) downto 0)
                                         (for diff = (- (aref nums i) (aref nums j)))
                                         (while (<= diff 3))
                                         (sum (rec j)))))
                          (setf (gethash i cache) res)
                          res)))))
      (rec (- (length nums) 1)))))

(defun part-1 ()
  (joltage-differences (read-lines)))

(defun part-2 ()
  (let ((ns (-<> (read-lines) (mapcar #'parse-integer <>) (sort <> #'<) (cons 0 <>))))
    (->> (reduce #'max ns) (+ 3) (list) 
         (append ns) (map 'vector #'identity) (ways-to-join))))
