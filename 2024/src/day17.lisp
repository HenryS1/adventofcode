(defpackage :day17
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :queue
   :fixnum-pq
   :anaphora 
   :metabang-bind)
  (:export 
   :calibration-value
   :total-calibration-value
   :digits-in-line
   :all-calibration-values))

(in-package :day17)

(defun ints (line) 
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" line)))

(defstruct computer (a 0 :type fixnum) (b 0 :type fixnum) (c 0 :type fixnum) 
           (program (make-array 0 :element-type '(unsigned-byte 8))
            :type (simple-array (unsigned-byte 8))))

(defun read-input (file)
  (with-open-file (f file)
    (let ((a (car (ints (read-line f))))
          (b (car (ints (read-line f))))
          (c (car (ints (read-line f)))))
      (read-line f)
      (let* ((instructions (ints (read-line f)))
             (program (make-array (length instructions) :element-type '(unsigned-byte 8))))
        (loop for instruction in instructions 
              for i from 0
              do (setf (aref program i) instruction))
        (make-computer :a a :b b :c c :program program)))))

(defun combo-operand (operand a b c)
  (case operand
    (4 a)
    (5 b)
    (6 c)
    (7 (error "invalid"))
    (t operand)))

(defun run-program (computer)
  (let ((a (computer-a computer))
        (b (computer-b computer))
        (c (computer-c computer))
        (program (computer-program computer)))
    (loop with i = 0
          with output = nil
          while (< i (length program))
          for instruction = (aref program i)
          for operand = (aref program (+ i 1))
          do (case instruction
               (0 (let ((result (floor a (ash 1 (combo-operand operand a b c)))))
                    (setf a result)
                    (incf i 2)))
               (1 (setf b (logxor b operand)) 
                (incf i 2))
               (2 (setf b (mod (combo-operand operand a b c) 8))
                (incf i 2))
               (3 (if (/= a 0)
                      (setf i operand)
                      (incf i 2)))
               (4 (setf b (logxor b c))
                (incf i 2))
               (5 (push (mod (combo-operand operand a b c) 8) output)
                (incf i 2))
               (6 (let ((result (floor a (ash 1 (combo-operand operand a b c)))))
                    (setf b result)
                    (incf i 2)))
               (7 (let ((result (floor a (ash 1 (combo-operand operand a b c)))))
                    (setf c result)
                    (incf i 2))))
          finally (return output))))

(defun part1 ()
  (let ((computer (read-input "day17input")))
   (reverse (run-program computer))))

(defun prefix-equal (one other n)
  (loop for i from 1 to n
        for a in one for b across other
        when (/= a b)
          do (return nil)
        finally (return t)))

(defun find-periodic-digits (computer)
  (let ((target (reverse (computer-program computer))))
    (labels ((rec (target-index increment a base-a selected-indices)
               (if (= target-index (length target))
                   a
                   (loop for i from 0 
                         for index-diff = (loop for i in selected-indices 
                                                for j = 8 then (* j 8)
                                                summing (* i j))
                         do (setf (computer-a computer) (+ base-a i index-diff))
                            (let ((result (run-program computer)))
                              (setf (computer-a computer) (+ base-a i increment))
                              (when (not (prefix-equal result target target-index))
                                (return nil))
                              (when (every #'= result target)
                                (let ((result (rec (+ target-index 1) 
                                                   (* increment 8)
                                                   (+ base-a i index-diff)
                                                   (+ base-a increment)
                                                   (cons i selected-indices))))
                                  (when result
                                    (return result)))))
                            (setf (computer-a computer) base-a)))))
      (rec 0 7 1 1 nil))))

(defun part2 ()
  (let* ((computer (read-input "day17input")))
    (find-periodic-digits computer)))
