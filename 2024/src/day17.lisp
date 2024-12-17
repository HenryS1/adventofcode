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
                        (format t "A ~a B ~a C ~a OUTPUT ~a~%" a b c output)
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
                    (format t "A ~a C ~a OPERAND ~a COMBO OPERAND ~a DIV ~a RESULT ~a~%" a c operand (combo-operand operand a b c) (ash 1 (combo-operand operand a b c)) result)
                    (setf c result)
                    (incf i 2))))
             (format t "INSTRUCTION ~a OPERAND ~a A ~a B ~a C ~a~%" instruction operand a b c)
          finally (return output))))

(defun part1 ()
  (let ((computer (read-input "day17input")))
    (reverse (run-program computer))))

(defun find-a (computer)
  (let* ((init-b (computer-b computer))
         (init-c (computer-c computer))
         (program (computer-program computer))
         (target (reverse (map 'list #'identity program))))
    (loop with b = init-b
          with c = init-c
          for a-init from 0
          for a = a-init
          for iter-output = (loop with i = 0
                             with output = nil
                             while (< i (length program))
                             for instruction = (aref program i)
                             for operand = (aref program (+ i 1))
                             do (case instruction
                                  (0 (let ((result (floor a 
                                                          (ash 1 (combo-operand operand a b c)))))
                                       (setf a result)
                                       (incf i 2)))
                                  (1 (setf b (logxor b operand)) 
                                   (incf i 2))
                                  (2 (setf b (mod (combo-operand operand a b c) 8))
                                   (incf i 2))
                                  (3 (if (/= a 0)
                                         (progn 
                                           (setf i operand))
                                         (incf i 2)))
                                  (4 (setf b (logxor b c))
                                   (incf i 2))
                                  (5 (push (mod (combo-operand operand a b c) 8) output)
                                   (incf i 2))
                                  (6 (let ((result (floor a
                                                          (ash 1 (combo-operand operand a b c)))))
                                       (setf b result)
                                       (incf i 2)))
                                  (7 (let ((result (floor a
                                                          (ash 1 (combo-operand operand a b c)))))
                                       (setf c result)
                                       (incf i 2))))
                             finally (return output))
;          do (format t "A ~a~%" a-init)
          ;; when (= a-init 117440)
          ;;   do (format t "EXPECTED OUTPUT ~a TARGET ~a~%" iter-output target)
          ;; when (equal iter-output target)
          do (format t "OUTPUT ~a TARGET ~a~%" iter-output target)
          until (equal iter-output target)
          finally (return a-init))))

(defun find-periodic-digits (computer)
  (loop with increment = 1
        for a = 1 then (+ a increment)
        while (< increment (expt 8 16))
        do (setf (computer-a computer) a)
           (format t "OUTPUT ~a~%" (run-program computer))
        when (= (mod a (* 8 increment)) 0)
          do (setf increment (* 8 increment))))

(defun choose-one-from-each (lists)
  (if (null lists)
      (list nil)
      (loop for fst in (car lists)
            appending (loop for rest in (choose-one-from-each (cdr lists))
                            collect (cons fst rest)))))

(defun agree (one other another)
  (and (= (logand (car one) 1) 
          (ash (cdr other) -2))
       (= (logand (car one) 3)
          (ash (cdr another) -1))))

(defun find-pair-agreement-2 (pair-lists target)
  (format t "PAIR LISTS ~a~%" pair-lists)
  (labels ((rec (remaining result len)
             (format t "REMAINING ~a~%" remaining)
             (if (null remaining)
                 (reverse result)
                 (loop for other in (car remaining)
                       for a-so-far = (make-a (reverse (cons other result)))
                       for digits = (find-digits a-so-far len)
                       do (format t "ACC ~a A SO FAR ~a DIGITS ~a TARGET ~a~%" (cons other result) a-so-far digits target)
                       when (every #'= digits target)
                         do (let ((rest (rec (cdr remaining) (cons other result) (+ len 1))))
                              (when rest (return rest)))))))
    (loop for one in (car pair-lists)
          for result = (rec (cdr pair-lists) (list one) 1)
                       ;; (loop for other in (cadr pair-lists)
                       ;;       for inner-result = (rec one other (cdr pair-lists) (list one))
                       ;;       when inner-result do (return inner-result))
          when result
            do (return result))
    ))

(defun find-pair-agreement (pair-lists)
  (labels ((rec (one other remaining result)
             (format t "REMAINING ~a~%" remaining)
             (if (null remaining)
                 (reverse result)
                 (loop for another in (car remaining)
                       when (agree one other another)
                         do (return (rec other another (cdr remaining) (cons other result)))))))
    (loop for one in (car pair-lists)
          for result = (loop for other in (cadr pair-lists)
                             for inner-result = (rec one other (cdr pair-lists) (list one))
                             when inner-result do (return inner-result))
          when result
            do (return result))))

(defun pair-to-number (pair offset)
  (logior (ash (car pair) (+ (logxor (cdr pair) 5) offset))
          (ash (cdr pair) offset)))

(defun make-a (pairs)
  (loop with a = 0
        for pair in pairs 
        for offset = 0 then (+ offset 3)
        do (setf a (logior a (pair-to-number pair offset)))
        finally (return a)))

(defun pair-less (one other)
  (or (< (car one) (car other))
      (and (= (car one) (car other))
              (< (cdr one) (cdr other)))))

(defun find-potential-pairs (computer)
  (loop for n across (computer-program computer)
        collect (loop for i from 0 to 7 
                      appending (loop for j from 0 to 7
                                      when (= (logxor i j 3) n)
                                        collect (cons i j)))))

(defun compute-a (computer)
  (let ((pairs (find-potential-pairs computer)))
    pairs
;    (find-pair-agreement pairs)
    ))

(defun find-digits (a n)
  (loop with digits = nil
        with b = 0
        with c = 0
        for i from 1 to n
        do (setf b (mod a 8))
           (setf b (logxor b 5))
           (setf c (ash a (- b)))
           (setf a (floor a 8))
           (setf b (logxor b c))
           (setf b (logxor b 6))
        do (push (mod b 8) digits)
           (format t "A ~a B ~a C ~a OUTPUT ~a~%" a b c digits)
        while (> a 0)
        finally (return digits)))

(defun part2 ()
  (let* ((computer (read-input "day17input"))
         (pairs (find-potential-pairs computer)))
    (find-pair-agreement-2 pairs (computer-program computer))))
