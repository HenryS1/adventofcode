(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :anaphora)
(ql:quickload :metabang-bind)
(ql:quickload :alexandria)

(defpackage :day8
  (:use :cl :cl-ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day8)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun collect-layers ()
  (iter (with line = (with-open-file (f "input8") (read-line f)))
        (for i first 0 then (+ i (* 25 6)))
        (while (< i (length line)))
        (collect (iter (for r from 0 to 5)
                       (collect
                           (iter (for c from 0 to 24)
                                 (collect (digit-char-p (aref line (+ i (* r 25) c))))))))))

(defun count-digit (layer digit)
  (reduce #'+ (mapcar (lambda (l) (count-if (lambda (e) (= e digit)) l)) layer)))

(defun find-min-zeros (layers)
  (iter (for layer in layers)
        (reducing layer by (lambda (new old) (if (< (count-digit new 0) (count-digit old 0))
                                              new old)))))

(defun answer-1 ()
  (let ((min-zeros (find-min-zeros (collect-layers))))
    (* (count-digit min-zeros 1)
       (count-digit min-zeros 2))))

(defun find-final-image (layers)
  (iter (with image = (make-array '(6 25) :initial-element 2))
        (for layer in layers)
        (iter (for r from 0 to 5)
              (for row in layer)
              (iter (for c from 0 to 24)
                    (for digit in row)
                    (when (= (aref image r c) 2)
                      (setf (aref image r c) digit))))
        (finally (return image))))

(defun answer-2 ()  (find-final-image (collect-layers)))
