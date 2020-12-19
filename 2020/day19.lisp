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

(defpackage :day19
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day19)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines (file)
  (iter (for line in-file file using #'read-line)
        (collect line)))

(defun read-rule (line)
  (bind (((head body) (split ":" line))
         (clauses (split "\\|" body)))
    (if (some #'digit-char-p body)
        (cons (parse-integer head) (mapcar #'ints clauses))
        (cons (parse-integer head) (list (subseq body 2 3))))))

(defun read-rules-and-patterns (lines)
  (iter (with parse-table = (make-hash-table :test 'equal))
        (for line in lines)
        (match line
          ((ppcre "\\d+:")
           (for (head . body) = (read-rule line))
           (setf (gethash head parse-table) body))
          ((ppcre "\\w+")
           (collect line into patterns)))
        (finally (return (cons patterns parse-table)))))

(defun match-input (input parse-table)
  (labels ((rec (clause i)
             (cond ((null clause) (list i))
                   ((>= i (length input)) nil)
                   ((stringp clause)
                    (if (search clause input :start2 i :end2 (+ i (length clause)))
                        (list (+ i 1))
                        nil))
                   (t (iter outer
                            (for next-clause in (gethash (car clause) parse-table))
                            (iter (for next-i in (rec next-clause i))
                                  (in outer (appending (rec (cdr clause) next-i)))))))))
    (iter outer 
          (for clause in (gethash 0 parse-table))
          (iter (for i in (rec clause 0))
                (when (= i (length input))
                  (return-from outer i))))))

(defun count-matches (lines)
  (bind (((patterns . parse-table) (read-rules-and-patterns lines)))
    (iter (for pattern in patterns)
          (count (match-input pattern parse-table)))))

(defun part-1 ()
  (count-matches (read-lines "input19")))

(defun part-2 ()
  (count-matches (read-lines "input19_part2")))
