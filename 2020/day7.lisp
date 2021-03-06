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

(defpackage :day7
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day7)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input7" using #'read-line)
        (collect line)))

(defun read-bag (line)
  (bind ((contained (all-matches-as-strings "\\d+\\s+\\w+\\s+\\w+" line))
         (container (all-matches-as-strings "^\\w+\\s+\\w+" line)))
    (cons container (iter (for b in contained)
                          (match b ((ppcre "(\\d+)\\s+(\\w+\\s+\\w+)" (read n) name)
                                    (collect (list name n))))))))

(defun make-graph (lines)
  (iter (with graph = (make-hash-table :test 'equal))
        (for line in lines)
        (for (container . contained) = (read-bag line))
        (iter (for (name n) in contained)
              (push (car container) (gethash name graph)))
        (finally (return graph))))

(defun find-containing-gold (graph)
  (let ((seen (make-hash-table :test 'equal)))
    (labels ((dfs (name)
               (iter (for other in (gethash name graph))
                     (when (not (gethash other seen))
                       (setf (gethash other seen) t)
                       (dfs other)))))
      (dfs "shiny gold")
      (hash-table-count seen))))

(defun make-contains-graph (lines)
  (iter (with graph = (make-hash-table :test 'equal))
        (for line in lines)
        (for (container . contained) = (read-bag line))
        (setf (gethash (car container) graph) contained)
        (finally (return graph))))

(defun find-contained-in-count (graph)
  (let ((cache (make-hash-table :test 'equal)))
    (labels ((count-contained-in (name)
               (if (gethash name cache)
                   (gethash name cache)
                   (iter (for (other n) in (gethash name graph))
                         (summing (+ n (* n (count-contained-in other))) into total)
                         (finally (return (setf (gethash name cache) total)))))))
      (count-contained-in "shiny gold"))))

(defun part-1 () (-> (read-lines) (make-graph) (find-containing-gold)))
(defun part-2 () (-> (read-lines) (make-contains-graph) (find-contained-in-count)))
