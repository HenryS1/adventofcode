(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :iterate)
  (ql:quickload :cl-ppcre)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind))

(defpackage :day19
  (:use :cl :iterate :cl-ppcre :anaphora :bind))

(in-package :day19)

(defclass node ()
  ((children :accessor children :initarg :children :initform (make-array 52 :initial-element nil))
   (value :accessor value :initarg :value :initform nil)))

(let ((lookup (iter (with table = (make-hash-table))
                    (for c in-string "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
                    (for i from 0)
                    (setf (gethash c table) i)
                    (finally (return table)))))
  (defun index (c) 
    (gethash c lookup)))

(defmethod insert-node (s i v (node node))
  (cond ((= i (length s)) (progn (if (value node) 
                                     (push v (cdr (value node)))
                                     (setf (value node) (cons s (list v)))) node))
        ((< i (length s))
         (aif (aref (children node) (index (aref s i)))
              (insert-node s (+ i 1) v it)
              (setf (aref (children node) (index (aref s i)))
                    (insert-node s (+ i 1) v (make-instance 'node))))
         node)
        (t nil)))

(defmethod match (c (node node))
  (aref (children node) (index c)))

(defun read-input ()
  (let* ((lines (iter (for line in-file "input19" using #'read-line) (collect line)))
         (forward-mappings (make-instance 'node))
         (backward-mappings (make-instance 'node))
         (input (car (last lines))))
    (mapc (lambda (s) 
            (let ((parts (split "\\s+" s)))
              (insert-node (car parts) 0 (car (last parts)) forward-mappings)
              (insert-node (car (last parts)) 0 (car parts) backward-mappings)))
          (butlast (butlast lines)))
    (values forward-mappings input backward-mappings)))

(defmethod matches-at ((node node) input index)
  (iter (for nd first node then (match (aref input i) nd))
        (while nd)
        (for i from index)
        (when (value nd) 
          (collect (value nd)))
        (while (< i (length input)))))

(defmethod node-matches ((node node) input)
  (iter (for i from 0 to (- (length input) 1))
        (for ms = (matches-at node input i))
        (when ms 
          (collect (cons i (matches-at node input i))))))

(defun substitute-match (index fst snd input)
  (concatenate 'string (subseq input 0 index) 
               snd
               (subseq input (+ index (length fst)))))

(defun check-matches (index)
  (bind (((:values nd input) (read-input)))
    (matches-at nd input index)))

(defun part-one ()
  (bind (((:values nd input) (read-input)))
    (let ((ms (node-matches nd input)))
      (iter (with table = (make-hash-table :test 'equal))
            (for (i . sub-ms) in ms)
            (iter (for (fst . snds) in sub-ms)
                  (iter (for snd in snds)
                        (setf (gethash (substitute-match i fst snd input) table) t)))
            (finally (return (hash-table-count table)))))))

(defun new-substitutions (nd input seen)
  (let ((ms (node-matches nd input)))
    (iter outer
          (for (i . sub-ms) in ms)
          (iter (for (fst . snds) in sub-ms)
                (iter (for snd in snds)
                      (for sub = (substitute-match i fst snd input))
                      (when (not (gethash sub seen))
                        (in outer (collect sub))))))))

(defun read-input-two ()
  (iter (with lines = (iter (for line in-file "input19" using #'read-line) (collect line)))
        (for line in (butlast (butlast lines)))
        (for (fst _ snd) = (split "\\s+" line))
        (collect (cons snd fst) into mappings)
        (finally (return (values (sort mappings
                                       (lambda (one other) (> (length (car one)) 
                                                              (length (car other)))))
                                 (car (last lines)))))))

(defun greedy-backward-search ()
  (bind (((:values mappings input) (read-input-two)))   
    (labels ((rec (s moves) 
               (if (string= s "e") 
                   moves
                   (iter (for (fst . snd) in mappings)
                         (awhen (and (scan fst s) (rec (regex-replace fst s snd) (+ moves 1)))
                           (return it))))))
      (rec input 0))))

