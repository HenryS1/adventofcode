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
                      (progn 
;                        (format t "A ~a B ~a C ~a OUTPUT ~a~%" a b c output)
                        (setf i operand))
                      (incf i 2)))
               (4 (setf b (logxor b c))
                (incf i 2))
               (5 (push (mod (combo-operand operand a b c) 8) output)
                (incf i 2))
               (6 (let ((result (floor a (ash 1 (combo-operand operand a b c)))))
                    (setf b result)
                    (incf i 2)))
               (7 (let ((result (floor a (ash 1 (combo-operand operand a b c)))))
;                    (format t "A ~a C ~a OPERAND ~a COMBO OPERAND ~a DIV ~a RESULT ~a~%" a c operand (combo-operand operand a b c) (ash 1 (combo-operand operand a b c)) result)
                    (setf c result)
                    (incf i 2))))
    ;         (format t "INSTRUCTION ~a OPERAND ~a A ~a B ~a C ~a~%" instruction operand a b c)
          finally (return output))))

(defun part1 ()
  (let ((computer (read-input "day17input")))
    (run-program computer)))

(defun find-periodic-digits (computer)
  (let ((target (reverse (computer-program computer))))
    (loop with indices = nil
          for target-index from 0
          for target-number across target
          for base-a = 1 then (+ base-a increment)
          for increment = 7 then (* 8 increment)
          for next-index = (loop for i from 0 
                                 for index-diff = (loop for (i . increment-product)
                                                          in indices 
                                                        for j = increment-product 
                                                          then (* j increment-product)
                                                        do (format t "I ~a J ~a~%" i j)
                                                        summing (* i j))
                                 do (format t "INCREMENT ~a INDICES ~a~%" increment indices)
                                    (format t "BASE A ~a INDEX ~a INDEX DIFF ~a~%" base-a i index-diff)
                                    (setf (computer-a computer) (+ base-a i index-diff))
                                    (let ((result (run-program computer)))
                                      (format t "A ~a TARGET INDEX ~a TARGET NUMBER ~a OUTPUT ~a~%" 
                                              (+ base-a i index-diff) target-index target-number result)
                                      (setf (computer-a computer) (+ base-a i increment))
                                      (format t "PLUS INCREMENT ~a ~a~%" 
                                              (+ base-a i increment)
                                              (run-program computer))
                                      (when (= (nth target-index result) target-number)
                                        (when (= target-index (- (length target) 1))
                                          (format t "A ~a~%" (+ base-a i index-diff)))
                                        (return i)))
                                    (setf (computer-a computer) base-a))
          do (push (cons next-index 8) indices)
          do (format t "A ~a OUTPUT ~a~%" base-a (run-program computer))
             ;; when (= (mod a (* 8 increment)) 0)
             ;;   do (setf increment (* 8 increment))
          finally (return indices))))

(defun print-a-output ()
  (let ((computer (read-input "day17input")))
    (loop for a from 0 to 40000
          do (setf (computer-a computer) a)
             (format t "OUTPUT ~a~%" (run-program computer)))))

(defun run-with-a (a)
  (let* ((computer (read-input "day17input")))
    (setf (computer-a computer) a)
    (run-program computer)))

(defun part2 ()
  (let* ((computer (read-input "day17input")))
    (find-periodic-digits computer)))
