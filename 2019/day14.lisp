(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day14
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day14)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun parse-reaction (line)
  (bind (((left right) (split "=>" (regex-replace-all "," line ""))))
    (list (read-syms (string-trim " " right)) (read-syms (string-trim " " left)))))

(defun collate-reactions (reactions)
  (iter (with table = (make-hash-table :test 'equal))
        (for ((n chem) ingredients) in reactions)
        (setf (gethash chem table) (cons n ingredients))
        (finally (return table))))

(defun read-reactions ()
  (iter (for line in-file "input14" using #'read-line)
        (collect (parse-reaction line))))

(defun find-min-ore (reactions &optional (available-chem (make-hash-table :test 'equal)))
  (let ((table (collate-reactions reactions)))
    (labels ((rec (chem quantity)
               (let ((decrement (min quantity (or (gethash chem available-chem) 0))))
                 (decf quantity decrement)
                 (when (gethash chem available-chem)
                   (decf (gethash chem available-chem) decrement)))
               (cond ((equal chem 'ORE) quantity)                   
                     ((= quantity 0) 0)
                     (t (bind (((n . ingredients) (gethash chem table))
                               (obtained (* (ceiling quantity n) n))
                               (wasted (- obtained quantity))
                               (cost (iter (for rest first ingredients then (nthcdr 2 rest))
                                           (while rest)
                                           (for ni = (first rest))
                                           (for chemi = (second rest))
                                           (sum (rec chemi (* (ceiling quantity n) ni))))))
                          (setf (gethash chem available-chem)
                                (+ wasted (or (gethash chem available-chem) 0)))
                          cost)))))
      (rec 'FUEL 1))))

(defun answer-1 () 
  (find-min-ore (read-reactions)))

(defun comp-avail (one other)
  (or (< (cdr one) (cdr other))
      (and (= (cdr one) (cdr other))
           (string< (symbol-name (car one)) (symbol-name (car other))))))

(defun available-key (available-chem)
  (sort (iter (for (k v) in-hashtable available-chem) (when (> v 0) (collect (cons k v))))
        #'comp-avail))

(defun adjust-available (available multiplier)
  (iter (for (k v) in-hashtable available)
        (setf (gethash k available) (* multiplier v)))
  available)

(defun exhaust-ore (ore &optional (available-chem (make-hash-table :test 'equal)))
  (iter (with reactions = (read-reactions))
        (for i from 0)
        (for previous = (copy-hash-table available-chem))
        (for cost = (find-min-ore reactions available-chem))
        (until (< (- ore cost) 0))
        (decf ore cost)
        (finally (return (list i ore previous)))))

(defun answer-2 ()
  (bind (((fuel ore-left available-chem) (exhaust-ore 10000000000))
         ((extra-fuel rem-ore final-available) 
            (exhaust-ore (+ (* 99 ore-left) 10000000000)
                         (adjust-available available-chem 99))))
    (list (+ (* 99 fuel) extra-fuel) rem-ore (available-key final-available))))
