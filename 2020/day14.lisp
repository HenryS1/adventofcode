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

(defpackage :day14
  (:use :cl :cl-ppcre :cl-arrows :trivia :trivia.ppcre
        :iterate :alexandria :anaphora :metabang-bind))

(in-package :day14)

(defun ints (line)
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-lines ()
  (iter (for line in-file "input14" using #'read-line)
        (collect line)))

(defun update-address (mask address value mem)
  (iter (with res = 0)
        (for ch in-string mask)
        (for i first 1 then (ash i 1))
        (cond ((char= ch #\X) (setf res (logior res (logand i value))))
              ((char= ch #\1) (setf res (logior res i))))
        (finally (setf (gethash address mem) res))))

(defun process-lines (lines)
  (iter (with mem = (make-hash-table))
        (with mask = "")
        (for line in lines)
        (match line
          ((ppcre "mask = (\\w+)" new-mask) (setf mask (reverse new-mask)))
          ((ppcre "mem\\[(\\d+)\\] = (\\d+)" (read address) (read value))
           (update-address mask address value mem)))
        (finally (return (iter (for (_ v) in-hashtable mem)
                               (summing v))))))

(defun update-mask-addresses (mask address value mem)
  (labels ((rec (i out-address)
             (cond ((>= i (length mask))
                    (setf (gethash out-address mem) value))
                   ((char= (aref mask i) #\1)
                    (rec (+ i 1) (logior out-address (ash 1 i))))
                   ((char= (aref mask i) #\0)
                    (rec (+ i 1) out-address))
                   ((char= (aref mask i) #\X)
                    (rec (+ i 1) (logxor out-address (logand out-address (ash 1 i))))
                    (rec (+ i 1) (logior out-address (ash 1 i)))))))
    (rec 0 address)))

(defun process-lines-address (lines)
  (iter (with mem = (make-hash-table))
        (with mask = "")
        (for line in lines)
        (match line
          ((ppcre "mask = (\\w+)" new-mask) (setf mask (reverse new-mask)))
          ((ppcre "mem\\[(\\d+)\\] = (\\d+)" (read address) (read value))
           (update-mask-addresses mask address value mem)))
        (finally (return (iter (for (_ v) in-hashtable mem)
                               (summing v))))))

(defun part-1 ()
  (process-lines (read-lines)))

(defun part-2 ()
  (process-lines-address (read-lines)))
